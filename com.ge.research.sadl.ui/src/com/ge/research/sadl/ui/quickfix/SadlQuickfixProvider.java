package com.ge.research.sadl.ui.quickfix;

import java.util.Map;
import java.io.StringReader;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.XtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModification;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.model.edit.ISemanticModification;
import org.eclipse.xtext.ui.editor.quickfix.DefaultQuickfixProvider;
import org.eclipse.xtext.ui.editor.quickfix.Fix;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.parser.antlr.SadlParser;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.sadl.ModelName;
import com.ge.research.sadl.sadl.PropValPartialTriple;
import com.ge.research.sadl.sadl.ResourceByName;
import com.ge.research.sadl.ui.contentassist.SadlProposalProvider;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.builder.ResourceManager;
import com.ge.research.sadl.validation.SadlJavaValidator;
import com.google.common.collect.Maps;
import com.google.inject.Inject;

/**
 * Provides quick fixes for problems found by the validator.
 */
public class SadlQuickfixProvider extends DefaultQuickfixProvider {

    // We need content assist for model names' baseUris.
    private final SadlProposalProvider proposalProvider;

    @Inject
	private SadlModelManagerProvider sadlModelManagerProvider;
    
    @Inject
    private SadlParser parser;

    @Inject
    public SadlQuickfixProvider(SadlProposalProvider proposalProvider) {
        this.proposalProvider = proposalProvider;
    }

	@Fix(SadlJavaValidator.MISSING_MODEL_NAME)
	public void provideModelName(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Add Model Name", "Add a missing model name", null, new ISemanticModification() {
			public void apply(EObject element, IModificationContext context)
					throws Exception {
				URI modelURI = element.eResource().getURI();
			    String baseUri = proposalProvider.generateBaseUri(modelURI);
				((ModelName)element).setBaseUri(baseUri);
			}
		});
	}
 
	@Fix(SadlJavaValidator.MISSING_HTTP_PREFIX)
	public void missingHttpPrefix(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Add Prefix", "Add 'http://' to the begining of the model name", null, new IModification() {
			public void apply(IModificationContext context) throws BadLocationException {
				IXtextDocument xtextDocument = context.getXtextDocument();
				String openingQuote = xtextDocument.get(issue.getOffset() + 1, 1);
				xtextDocument.replace(issue.getOffset() + 1, 1, openingQuote + "http://");
			}

		});
	}

	@Fix(SadlJavaValidator.DUPLICATE_MODEL_NAME)
	public void replaceModelName(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Replace Model Name", "Replace a duplicate model name", null, new ISemanticModification() {
			public void apply(EObject element, IModificationContext context)
					throws Exception {
				URI modelURI = element.eResource().getURI();
			    String baseUri = proposalProvider.generateBaseUri(modelURI);
				((ModelName)element).setBaseUri(baseUri);
			}
		});
	}
 
//	@Fix(SadlJavaValidator.ADD_URI_END_CHAR)
//	public void addUriEndChar(final Issue issue, IssueResolutionAcceptor acceptor) {
//		acceptor.accept(issue, "Make Valid URI", "Add a valid end character to the model name", null, new IModification() {
//			public void apply(IModificationContext context) throws BadLocationException {
//				IXtextDocument xtextDocument = context.getXtextDocument();
//				String closingQuote = xtextDocument.get(issue.getOffset() + issue.getLength() - 1, 1);
//				xtextDocument.replace(issue.getOffset() + issue.getLength() - 1, 1, "#" + closingQuote);
//			}
//
//		});
//	}

	@Fix(SadlJavaValidator.ADD_MODEL_VERSION)
	public void addModelVersion(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Add Model Version", "Add a model version with CVS tags", null, new ISemanticModification() {
			public void apply(EObject element, IModificationContext context)
					throws Exception {
				// this string can't be complete or Source Code Control may modify it
				String versionTemplate = "$";
				versionTemplate += "Revision: ";
				versionTemplate += "$ Last modified on $";
				versionTemplate += "Date: ";
				versionTemplate += "$";
				((ModelName)element).setVersion(versionTemplate);
			}
		});
	}
	
	@Fix(SadlJavaValidator.ADD_GLOBAL_ALIAS)
	public void addModelGlobalAlias(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Add Model Global Alias", "Add a global alias to the model", null, new IModification() {

			@Override
			public void apply(IModificationContext context) throws Exception {
				final IXtextDocument doc = context.getXtextDocument();
				// assumption: that URI is in same line as 'uri' keyword
				// parse the line to create a ModelName
				String line = doc.get(issue.getOffset(), doc.getLineLength(issue.getLineNumber()-1));
				IParseResult parseResult = parser.parse(parser.getGrammarAccess().getModelNameRule(), new StringReader(line));
				ModelName name = (ModelName) parseResult.getRootASTElement();

				// compute alias name
				URI uri = URI.createURI(name.getBaseUri());
				String galias = (uri.segmentCount()>1) ? uri.lastSegment() : "aliasName";

				// find insertion offset: Search for end of URI string 
				int offset = line.indexOf(name.getBaseUri())+name.getBaseUri().length();
				// is uri string surrounded by " or ' ?
				char stringDelimiter = line.substring(0,offset).indexOf('"')>0 ? '"' : '\'';

				// find position of end character
				offset = line.indexOf(stringDelimiter, offset)+1;

				doc.replace(offset, 0, " alias "+galias);
			}
		});
	}
	
	@Fix(SadlJavaValidator.MISSING_ALIAS)
	public void addImportAlias(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Add import model alias", "Add an alias for the imported model", null, new ISemanticModification() {
			public void apply(EObject element, IModificationContext context)
					throws Exception {
				String alias = null;
				if (element instanceof com.ge.research.sadl.sadl.Import) {
					String uri = ((com.ge.research.sadl.sadl.Import)element).getImportURI().trim();
					if (uri.endsWith("/")) {
						uri = uri.substring(0, uri.length() - 1);
					}
					int ls = uri.lastIndexOf('/');
					if (ls > 0) {
						alias = uri.substring(ls + 1);
					}
				}
				if (alias == null) {
					alias = "aliasName";
				}
				((com.ge.research.sadl.sadl.Import)element).setAlias(alias);
			}
		});
	}

	@Fix(SadlJavaValidator.DOUBLE_ALIAS)
	public void removeImportAlias(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Remove import model alias", "Remove local alias for the model import", null, new ISemanticModification() {
			public void apply(EObject element, IModificationContext context)
					throws Exception {
				((com.ge.research.sadl.sadl.Import)element).setAlias(null);
			}
		});
	}

	@Fix(SadlJavaValidator.IMPROPER_IMPORT_FILE_URI)
	public void replaceImportUri(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Use SADL file name", "Replace with import SADL file name", null, new ISemanticModification() {
			public void apply(EObject element, IModificationContext context)
					throws Exception {
				String newUri = ((com.ge.research.sadl.sadl.Import)element).getImportURI();
				if (newUri.startsWith("file:")) {
					newUri = newUri.substring(5);
					while (newUri.startsWith("/")) {
						newUri = newUri.substring(1);
					}
				}
				((com.ge.research.sadl.sadl.Import)element).setImportURI(newUri);
			}
		});
		acceptor.accept(issue, "Use model URI", "Replace with import model URI", null, new ISemanticModification() {
			public void apply(EObject element, IModificationContext context)
					throws Exception {
				String uri = ((com.ge.research.sadl.sadl.Import)element).getImportURI();
    			if (uri.endsWith(ResourceManager.SADLEXTWITHPREFIX)) {
    				if (uri.startsWith(ResourceManager.FILE_URL_PREFIX)) {
		    			int idx = ResourceManager.FILE_URL_PREFIX.length();
		    			while (idx < uri.length() && uri.charAt(idx) == '/') {
		    				idx++;
		    			}
		    			uri = uri.substring(idx);
    				}
    				SadlModelManager visitor = sadlModelManagerProvider.get(element.eResource());
					URI projectUri = ResourceManager.getProjectUri(visitor.getModelResource().getURI());
					SadlUtils su = new SadlUtils();
					URI sadlUri = URI.createURI(su.fileNameToFileUrl(ResourceManager.findSadlFileInProject(projectUri.toFileString(), uri)));
					URI owlUri = ResourceManager.validateAndReturnOwlUrlOfSadlUri(sadlUri);
					uri = visitor.getConfigurationMgr(ResourceManager.getOwlModelsFolder(sadlUri)).findPublicUriOfOwlFile(owlUri.toString());
					((com.ge.research.sadl.sadl.Import)element).setImportURI(uri);
	    		}
			}
		});
	}

	@Fix(SadlJavaValidator.ONTCLASS_NOT_DEFINED)
	public void addClassDefinition(final Issue issue, IssueResolutionAcceptor acceptor) {
		TemplateIssueResolutionAcceptor acc = (TemplateIssueResolutionAcceptor) acceptor;
		acc.accept(issue, acc.findTemplate("class"), new TemplateContextSupplier() {
			public Map<String, String> getVariables(IDocument doc,IQuickAssistInvocationContext cactx) throws Exception {
				String issueString = doc.get(issue.getOffset(), issue.getLength());
				Map<String,String> r = Maps.newHashMap();
				r.put("ClassName", issueString);
				return r;
			}
			
			public Position getPosition(IDocument doc,IQuickAssistInvocationContext cactx) throws Exception {
				return getAfterStatementPosition(doc, cactx, issue);
			}
		});
		acc.accept(issue, acc.findTemplate("class-subtype"), new TemplateContextSupplier() {
			public Map<String, String> getVariables(IDocument doc,IQuickAssistInvocationContext cactx) throws Exception {
				String issueString = doc.get(issue.getOffset(), issue.getLength());
				Map<String,String> r = Maps.newHashMap();
				r.put("ClassName", issueString);
				return r;
			}

			public Position getPosition(IDocument doc,IQuickAssistInvocationContext cactx) throws Exception {
				return getAfterStatementPosition(doc, cactx, issue);
			}
		});		
	}

	@Fix(SadlJavaValidator.OBJECTPROPERTY_NOT_DEFINED)
	public void addObjectPropertyDefinition(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Add Property", "Add missing object property definition to the model", null, new ISemanticModification() {
			public void apply(EObject rsrcId, IModificationContext context) throws BadLocationException {
				IXtextDocument xtextDocument = context.getXtextDocument();
				if (xtextDocument instanceof XtextDocument) {
					Object[] fixInfo = prepareMissingConceptFix(xtextDocument, rsrcId, issue);
					xtextDocument.replace(((Integer)fixInfo[1]).intValue(), 1, "." + System.getProperty("line.separator") + (fixInfo[0] != null ? fixInfo[0] : "New_Prop") + " describes .\n");
				}
			}

		});
	}
	
	@Fix(SadlJavaValidator.DATATYPEPROPERTY_NOT_DEFINED)
	public void addDatatypePropertyDefinition(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Add Property", "Add missing data property definition to the model", null, new ISemanticModification() {
			public void apply(EObject rsrcId, IModificationContext context) throws BadLocationException {
				IXtextDocument xtextDocument = context.getXtextDocument();
				if (xtextDocument instanceof XtextDocument) {
					Object[] fixInfo = prepareMissingConceptFix(xtextDocument, rsrcId, issue);
					xtextDocument.replace(((Integer)fixInfo[1]).intValue(), 1, "." + System.getProperty("line.separator") + (fixInfo[0] != null ? fixInfo[0] : "New_Prop") + " describes .\n");
				}
			}

		});
	}

	@Fix(SadlJavaValidator.INSTANCE_NOT_DEFINED)
	public void addInstanceDefinition(final Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Declare Instance", "Add missing instance declaration to the model", null, new ISemanticModification() {
			public void apply(EObject rsrcId, IModificationContext context) throws BadLocationException {
				IXtextDocument xtextDocument = context.getXtextDocument();
				if (xtextDocument instanceof XtextDocument) {
					Object[] fixInfo = prepareMissingConceptFix(xtextDocument, rsrcId, issue);
					xtextDocument.replace(((Integer)fixInfo[1]).intValue(), 1, "." + System.getProperty("line.separator") + (fixInfo[0] != null ? fixInfo[0] : "New_Instance") + " is a " + fixInfo[2] + ".\n");
				}
			}

		});
		acceptor.accept(issue, "Declare Instance in-place", "Add missing instance declaration in-place", null, new ISemanticModification() {
			public void apply(EObject rsrcId, IModificationContext context) throws BadLocationException {
				IXtextDocument xtextDocument = context.getXtextDocument();
				if (xtextDocument instanceof XtextDocument) {
					int insertAt = issue.getOffset();
					Object[] fixInfo = prepareMissingConceptFix(xtextDocument, rsrcId, issue);
					xtextDocument.replace(insertAt, issue.getLength(), "(a " + fixInfo[2] + " " + xtextDocument.get(issue.getOffset(), issue.getLength()) + ")");
				}
			}

		});
	}

	private Object[] prepareMissingConceptFix(IXtextDocument xtextDocument, EObject conceptEobject, Issue issue) throws BadLocationException {
		String nameAt0 = null;
		String propName = null;
		String nameAt2 = null;
		if (conceptEobject instanceof ResourceByName) {
			nameAt0 = ((ResourceByName)conceptEobject).getName().getName();
			EObject container = conceptEobject.eContainer().eContainer();
			if (container instanceof PropValPartialTriple) {
				propName = ((ResourceByName)((PropValPartialTriple)container).getPropertyName()).getName().getName();
//				((SadlProposalProvider)proposalProvider).visitor.toString();
//				System.out.println("Property: " + propName);
			}
		}
		if (issue.getCode().equals(SadlJavaValidator.INSTANCE_NOT_DEFINED)) {
			SadlModelManager visitor = sadlModelManagerProvider.get(conceptEobject.eResource());
			if (visitor != null) {
				ConceptName rcn = null;
				try {
					rcn = visitor.getObjectPropertyRange(new ConceptName(propName));
				} catch (ConfigurationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (rcn != null) {
					nameAt2 = rcn.getName();
				}
				else {
					nameAt2 = "Class";
				}
			}
		}
		int length = ((XtextDocument)xtextDocument).getLength() - (issue.getOffset() + issue.getLength());
		String rest = xtextDocument.get(issue.getOffset() + issue.getLength(), length);
		int index = rest.indexOf('.');
		int insertionPt = issue.getOffset() + issue.getLength() + index;
		Object[] results = new Object[3];
		results[0] = nameAt0;
		results[1] = Integer.valueOf(insertionPt);
		results[2] = nameAt2;
		return results;
	}

	private Position getAfterStatementPosition(IDocument doc,
			IQuickAssistInvocationContext cactx, Issue issue) throws BadLocationException {
		int offset = issue.getOffset();
		int length = issue.getLength();
		int totalLen = doc.getLength();
		int afterLen;
		if (totalLen >= offset + length + 100) {
			afterLen = 100;
		}
		else {
			afterLen = totalLen - (offset + length);
		}
		String after = doc.get(offset + length, afterLen);
		boolean endOfStatementFound = false;
		boolean newLineFound = false;
		for (int i = 0; i < afterLen; i++) {
			String nextChar = after.substring(i, i + 1);
			// look for end of statement
			if (!endOfStatementFound) {
				if (nextChar.equals(".")) {
					endOfStatementFound = true;
				}
			}
			else {
				// after end of statement, look for new line (or carriage return)
				if (nextChar.equals("\n") || nextChar.equals("\r")) {
					// this is line separator, keep going
					newLineFound = true;
					continue;
				}
				else if (newLineFound && Character.isWhitespace(nextChar.charAt(0))) {
					// if we found new line and this is whitespace (not \r or \n), done
					afterLen = i + 1;
					break;
				}
				else if (!Character.isWhitespace(nextChar.charAt(0))) {
					// found end of statement and this isn't white space so it is after position
					afterLen = i;
					break;
				}
			}
		}
		Position pos = new Position(offset + length + afterLen, 0);
		return pos;
	}
	
}
