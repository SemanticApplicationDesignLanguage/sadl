/************************************************************************
 * Copyright © 2007-2010 - General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a 
 * language for building semantic models and expressing rules that 
 * capture additional domain knowledge. The SADL-IDE (integrated 
 * development environment) is a set of Eclipse plug-ins that 
 * support the editing and testing of semantic models using the 
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES 
 * and licensed under the Eclipse Public License - v 1.0 
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/22 20:32:50 $
 ***********************************************************************/

package com.ge.research.sadl.ui.contentassist;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.jface.text.templates.TemplateVariable;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.ui.editor.templates.AbstractTemplateVariableResolver;
import org.eclipse.xtext.ui.editor.templates.XtextTemplateContext;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ModelManager.ImportListType;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.reasoner.BuiltinInfo;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManagerForEditing.Scope;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.utils.SadlUtils.ConceptType;
import com.google.inject.Inject;

/**
 * Provides a list of names having the given conceptual type specified
 * as a parameter to the template variable. 
 */
public class SadlResourceNameTemplateVariableResolver extends
        AbstractTemplateVariableResolver {

    @Inject
    private SadlModelManager visitor;
    
    @Inject
    private SadlProposalProvider proposalProvider;

    /**
     * Initializes this resolver's type and description (those strings are
     * displayed in the Sadl template preference page when you select a
     * custom variable resolver).
     */
    public SadlResourceNameTemplateVariableResolver() {
        super("ResourceName", "Select a ResourceName with the given conceptual type, e.g., ${class:ResourceName('ONTCLASS')}");
    }

    /**
     * Looks up and returns a list of names having the given conceptual type.
     * 
     * @param variable Has a parameter specifying the conceptual type.
     * @param xtextTemplateContext Not needed by this method.
     * @return List of names having the given conceptual type.
     */
    @Override
    public List<String> resolveValues(TemplateVariable variable,
            XtextTemplateContext xtextTemplateContext) {

		String conceptualType = (String) variable.getVariableType()
				.getParams().iterator().next();
		if (conceptualType.equals("VERSION")) {
			List<String> values = new ArrayList<String>();
			values.add("$" + "Revision:" + "$ Last modified on   $" + "Date:" + "$");
			return values;
		}
		else if (conceptualType.equals("BUILTINNAME")) {
			ICompositeNode parent = xtextTemplateContext.getContentAssistContext().getCurrentNode().getParent();
//			System.out.println(parent.toString());

//			INode lastCompleteNode = xtextTemplateContext.getContentAssistContext().getLastCompleteNode();
//			ICompositeNode lcnc = lastCompleteNode.getParent();
//			System.out.println(lcnc.toString());
//			TreeIterator<EObject> contents = lcnc.eAllContents();
//			while (contents.hasNext()) {
//				EObject nextobj = contents.next();
////				System.out.println(nextobj.toString());
//			}
			
			List<String> values = new ArrayList<String>();
			String modelFolderName = null;
			try {
				ConfigurationManager configMgr = visitor.getConfigurationMgr(modelFolderName);
				if (configMgr instanceof ConfigurationManagerForIDE) {
					List<BuiltinInfo> builtins = ((ConfigurationManagerForIDE)configMgr).getAvailableBuiltinsForCurrentReasoner();
					if (builtins != null) {
						for (int i = 0; i < builtins.size(); i++) {
							String withArgs = builtins.get(i).getName() + "(";
							int argCnt = builtins.get(i).getNumArgs();
							int effectiveArgCount = argCnt;
							// this should include determination of whether this is in the premise (given, if)
							//	or in the conclusion. Conclusion rules do not assign values to output variables so all arguments
							//	should be retained. For a premise built-in, the number of output variables should be dropped
							//  from the syntax constructed for the content assist.
							if (argCnt > 1) {
								effectiveArgCount--;
							}
							if (argCnt <= 0) {
								withArgs += "v1,...";
							}
							else {
								for (int j = 0; j < effectiveArgCount; j++) {
									if (j > 0) {
										withArgs += ", ";
									}
									withArgs += "v" + (j + 1);
								}
							}
							withArgs += ")";
							if (values.size() > 0) {
								boolean added = false;
								for (int j = 0; j < values.size(); j++) {
									String nextele = values.get(j);
									if (nextele.compareToIgnoreCase(withArgs) > 0) {
										values.add(j, withArgs);
										added = true;
										break;
									}
								}
								if (!added) {
									values.add(withArgs);
								}
							}
							else {
								values.add(withArgs);
							}
						}
					}
				}
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (Throwable t) {
				t.printStackTrace();
			}
			if (values.size() == 0) {
				values.add("no_builtins_found");
			}
			return values;
		}

		ConceptType cType = null; //ANNOTATIONPROPERTY, DATATYPEPROPERTY, OBJECTPROPERTY, ONTCLASS, INDIVIDUAL
		String constraint = null;
		if (conceptualType.indexOf(':') > 0) {
			int colpos = conceptualType.indexOf(':');
			constraint = conceptualType.substring(colpos + 1);
			conceptualType = conceptualType.substring(0, colpos);
		}
		if (ConceptType.ANNOTATIONPROPERTY.toString().equals(conceptualType)) {
		    cType = ConceptType.ANNOTATIONPROPERTY;
		}
		else if (ConceptType.DATATYPEPROPERTY.toString().equals(conceptualType)) {
		    cType = ConceptType.DATATYPEPROPERTY;
		}
		else if (ConceptType.OBJECTPROPERTY.toString().equals(conceptualType)) {
		    cType = ConceptType.OBJECTPROPERTY;
		}
		else if (ConceptType.ONTCLASS.toString().equals(conceptualType)) {
		    cType = ConceptType.ONTCLASS;
		}
		else if (ConceptType.INDIVIDUAL.toString().equals(conceptualType)) {
		    cType = ConceptType.INDIVIDUAL;
		}
		else if (ConceptType.MODELNAME.toString().equals(conceptualType)) {
			cType = ConceptType.MODELNAME;
        	IPreferencesService service = Platform.getPreferencesService();
        	String value = service.getString("com.ge.research.sadl.Sadl", "importBy", "fn", null);
            if (value != null && !value.isEmpty()) {
            	visitor.setImportListType(value.equals("fn") ? ImportListType.NAME_AS_SADL_FILENAME : ImportListType.NAME_AS_URI);
            }
		}
		else {
		    cType = ConceptType.ONTCLASS;
		}

		List<String> values = new ArrayList<String>();
		try {
			if (visitor.getModelBaseUri() == null) {
				// set the model baseUri (from preferences) if it hasn't been set so that we can generate
				//	a default model name if needed
				visitor.setModelBaseUri(proposalProvider.generateBaseUri(visitor.getModelResource().getURI()));
			}
	        for (TreeIterator<EObject> iter = EcoreUtil.getAllContents(visitor.getModelResource(), true); visitor.getModelName() == null && iter.hasNext();) { 
	            EObject eObject = iter.next();
	            if (visitor.doSwitch(eObject) != null) {
	                iter.prune();
	            }
	        }
	        List<ConceptName> names = null;
	        if (constraint != null) {
	        	names = visitor.getNamedConceptsOfType(cType, Scope.INCLUDEIMPORTS, new ConceptName(constraint));
	        }
	        else {
	            names = visitor.getNamedConceptsOfType(cType, Scope.INCLUDEIMPORTS);
	        }
	        
	        if (names != null && names.size() > 0) {
	        	IPreferencesService service = Platform.getPreferencesService();
	        	boolean bRemoveUnnecessaryPrefixes = service.getBoolean("com.ge.research.sadl.Sadl", "prefixesOnlyAsNeeded", false, null);
			    if (!cType.equals(ConceptType.MODELNAME) && bRemoveUnnecessaryPrefixes) {
			    	SadlProposalProvider.removeUnnecessaryPrefixes(names);
			    }
	        }
	        
            for (ConceptName name : names) {
                values.add(name.toString());
            }
        } 
		catch (InvalidNameException e) {
            e.printStackTrace();
        } catch (ConfigurationException e) {
			e.printStackTrace();
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (values.size() == 0) {
			values.add("No_valid_completions_found");
		}
		else {
			// alphabetize the list
			Collections.sort(values);
		}
		return values;
    }

}
