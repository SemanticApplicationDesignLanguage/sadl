package com.ge.research.sadl.ui.syntaxcoloring;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightedPositionAcceptor;
import org.eclipse.xtext.ui.editor.syntaxcoloring.ISemanticHighlightingCalculator;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.services.SadlGrammarAccess;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.contentassist.SadlProposalProvider;
import com.ge.research.sadl.utils.SadlUtils.ConceptType;
import com.google.inject.Inject;

public class SadlSemanticHighlightingCalculator
        implements ISemanticHighlightingCalculator,
        IPartListener2, IResourceChangeListener {

    private final SadlGrammarAccess grammarAccess;
    private final SadlModelManager visitor;
    private final SadlProposalProvider proposalProvider;

    @Inject
    public SadlSemanticHighlightingCalculator(SadlGrammarAccess grammarAccess,
            SadlModelManager visitor, SadlProposalProvider proposalProvider) {
        this.grammarAccess = grammarAccess;
        this.visitor = visitor;
        this.proposalProvider = proposalProvider;
    }

    @Override
    public void provideHighlightingFor(XtextResource resource,
            IHighlightedPositionAcceptor acceptor) {

        if (resource == null)
            return;

        synchronized (visitor) {
	        // We have to convert the Ecore model to a Jena OntModel on the fly
	        // in order to look up which concept type an identifier has.
	        try {
	        	// assume that the editor is open--why else would we be in the semantic highlighting code??
	        	boolean editorOpen = true;
	        	String resourceName = resource.getURI().lastSegment();
	        	IWorkbenchWindow wndw = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
	        	ResourcesPlugin.getWorkspace().addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE | IResourceChangeEvent.PRE_CLOSE);
	        	if (wndw != null) {
	        		if (wndw.getActivePage() != null) {
		        		// can't set the listener for close unless we have a window...
						IEditorReference[] editors = wndw.getActivePage().getEditorReferences();
						for (int i = 0; editors != null && i < editors.length; i++) {
							IEditorReference editor = editors[i];
							if (resourceName.equals(editor.getPartName())) {
		//						editorOpen = true;
								editor.getPage().addPartListener(this);
							}
						}
	        		}
	        	}
				visitor.processModel(resource, false, editorOpen, null);
			} catch (CoreException e) {
	//			SadlConsole.writeToConsole(MessageType.ERROR, "Error processing model for syntax highlighting: " + e.getLocalizedMessage());
				e.printStackTrace();
			}
	        SadlConsole.displayMessages(visitor);

	        // Now we can highlight each node according to its type.
	        Iterable<INode> allNodes = resource.getParseResult().getRootNode().getAsTreeIterable();
	        for (INode abstractNode : allNodes) {
	            EObject grammarElement = abstractNode.getGrammarElement();
	            if (grammarElement == grammarAccess.getModelNameAccess()
	                    .getBaseUriSTRINGTerminalRuleCall_1_0()) {
	                highlightNode(abstractNode, SadlHighlightingConfiguration.URI_ID, acceptor);
	            }
	            else if (grammarElement == grammarAccess.getImportAccess()
	                    .getImportURISTRINGTerminalRuleCall_1_0()) {
	                highlightNode(abstractNode, SadlHighlightingConfiguration.URI_ID, acceptor);
	            }
	            else if (grammarElement == grammarAccess.getResourceNameAccess()
	                    .getNameNAMEParserRuleCall_0_0()) {
	                lookupAndHighlightName(abstractNode, acceptor);
	            }
	            else if (grammarElement == grammarAccess.getResourceNameAccess().getNameNAMEParserRuleCall_0_0()) {
	            	lookupAndHighlightName(abstractNode, acceptor);
	            }
	            else if (grammarElement == grammarAccess.getResourceByNameAccess()
	                    .getNameResourceNameCrossReference_0()) {
	                lookupAndHighlightName(abstractNode, acceptor);
	            }
	            else if (grammarElement == grammarAccess.getNUMBERAccess().getUNSIGNED_NUMBERTerminalRuleCall_1()
	            		|| grammarElement == grammarAccess.getNUMBERAccess().getHyphenMinusKeyword_0()) {
	            	highlightNode(abstractNode, SadlHighlightingConfiguration.NUMBER_ID, acceptor);
	            }
	        }
        }
    }

    private void highlightNode(INode abstractNode, String id, IHighlightedPositionAcceptor acceptor) {
        if (abstractNode == null)
            return;

        if (abstractNode instanceof ILeafNode) {
            acceptor.addPosition(abstractNode.getOffset(), abstractNode.getLength(), id);
        }
        else {
            for (ILeafNode leaf : abstractNode.getLeafNodes()) {
                if (!leaf.isHidden()) {
                    acceptor.addPosition(leaf.getOffset(), leaf.getLength(), id);
                }
            }
        }
    }

    private void lookupAndHighlightName(INode abstractNode, IHighlightedPositionAcceptor acceptor) {
        if (abstractNode == null)
            return;

        if (abstractNode instanceof ILeafNode) {
            String id = getHighlightingId(((ILeafNode) abstractNode).getText());
            acceptor.addPosition(abstractNode.getOffset(), abstractNode.getLength(), id);
        }
        else {
            // We have to concatenate a composite name from its tokens.
            StringBuilder sb = new StringBuilder();
            for (ILeafNode leaf : abstractNode.getLeafNodes()) {
                if (!leaf.isHidden()) {
                    sb.append(leaf.getText());
                }
            }
            // Look up the name's type and highlight it accordingly.
            String id = getHighlightingId(sb.toString());
            acceptor.addPosition(abstractNode.getOffset(), abstractNode.getLength(), id);
        }
    }

    private String getHighlightingId(String name) {
    	if (name.indexOf('^') >= 0) {
    		name = SadlModelManager.removeEscapeCharacters(name);
    	}
		int idx = name.indexOf(":");
		if (idx > 0) {
			if (name.charAt(idx + 1) == '^') {
				name = name.substring(0, idx + 1) + name.substring(idx + 2);
			}
		}
        ConceptType type = visitor.getConceptType(name);
        switch (type) {
        case ANNOTATIONPROPERTY:
            return SadlHighlightingConfiguration.ANNOTATION_PROPERTY_ID;
        case CONCEPT_NOT_FOUND_IN_MODEL:
//            return SadlHighlightingConfiguration.DEFAULT_ID;
        	return SadlHighlightingConfiguration.VARIABLE_ID;
        case DATATYPEPROPERTY:
            return SadlHighlightingConfiguration.DATA_PROPERTY_ID;
        case INDIVIDUAL:
            return SadlHighlightingConfiguration.INSTANCE_ID;
        case OBJECTPROPERTY:
            return SadlHighlightingConfiguration.OBJECT_PROPERTY_ID;
        case ONTCLASS:
            return SadlHighlightingConfiguration.CLASS_ID;
//        case RDFDATATYPE:
//        	return SadlHighlightingConfiguration.RDFDATATYPE_ID;
        default:
            return SadlHighlightingConfiguration.DEFAULT_ID;
        }
    }

	@Override
	public void partActivated(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		visitor.editorClosed(partRef.getPartName());
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {

	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		visitor.removeResourceModel(visitor.getModelResource());
	}

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		if (event.getType() == IResourceChangeEvent.PRE_CLOSE && event.getResource() instanceof IProject) {
			visitor.removeAllProjectResourceModels(event.getResource().getName());
		}
	}
}
