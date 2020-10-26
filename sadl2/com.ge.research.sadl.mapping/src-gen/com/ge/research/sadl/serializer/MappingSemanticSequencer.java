package com.ge.research.sadl.serializer;

import com.ge.research.sadl.mapping.Group;
import com.ge.research.sadl.mapping.Import;
import com.ge.research.sadl.mapping.LiteralValue;
import com.ge.research.sadl.mapping.MappingPackage;
import com.ge.research.sadl.mapping.Model;
import com.ge.research.sadl.mapping.NewModelNS;
import com.ge.research.sadl.mapping.Ref;
import com.ge.research.sadl.mapping.Triple;
import com.ge.research.sadl.services.MappingGrammarAccess;
import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.serializer.acceptor.ISemanticSequenceAcceptor;
import org.eclipse.xtext.serializer.diagnostic.ISemanticSequencerDiagnosticProvider;
import org.eclipse.xtext.serializer.diagnostic.ISerializationDiagnostic.Acceptor;
import org.eclipse.xtext.serializer.sequencer.AbstractDelegatingSemanticSequencer;
import org.eclipse.xtext.serializer.sequencer.GenericSequencer;
import org.eclipse.xtext.serializer.sequencer.ISemanticSequencer;
import org.eclipse.xtext.serializer.sequencer.ITransientValueService;

@SuppressWarnings("all")
public class MappingSemanticSequencer extends AbstractDelegatingSemanticSequencer {

	@Inject
	private MappingGrammarAccess grammarAccess;
	
	public void createSequence(EObject context, EObject semanticObject) {
		if(semanticObject.eClass().getEPackage() == MappingPackage.eINSTANCE) switch(semanticObject.eClass().getClassifierID()) {
			case MappingPackage.GROUP:
				if(context == grammarAccess.getGroupRule()) {
					sequence_Group(context, (Group) semanticObject); 
					return; 
				}
				else break;
			case MappingPackage.IMPORT:
				if(context == grammarAccess.getImportRule()) {
					sequence_Import(context, (Import) semanticObject); 
					return; 
				}
				else break;
			case MappingPackage.LITERAL_VALUE:
				if(context == grammarAccess.getLiteralValueRule()) {
					sequence_LiteralValue(context, (LiteralValue) semanticObject); 
					return; 
				}
				else break;
			case MappingPackage.MODEL:
				if(context == grammarAccess.getModelRule()) {
					sequence_Model(context, (Model) semanticObject); 
					return; 
				}
				else break;
			case MappingPackage.NEW_MODEL_NS:
				if(context == grammarAccess.getNewModelNSRule()) {
					sequence_NewModelNS(context, (NewModelNS) semanticObject); 
					return; 
				}
				else break;
			case MappingPackage.REF:
				if(context == grammarAccess.getRefRule()) {
					sequence_Ref(context, (Ref) semanticObject); 
					return; 
				}
				else break;
			case MappingPackage.TRIPLE:
				if(context == grammarAccess.getTripleRule()) {
					sequence_Triple(context, (Triple) semanticObject); 
					return; 
				}
				else break;
			}
		if (errorAcceptor != null) errorAcceptor.accept(diagnosticProvider.createInvalidContextOrTypeDiagnostic(semanticObject, context));
	}
	
	/**
	 * Constraint:
	 *     (groupLines+=Triple | groupLines+=Group)+
	 */
	protected void sequence_Group(EObject context, Group semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (importURI=STRING alias=ID?)
	 */
	protected void sequence_Import(EObject context, Import semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (literalNumber=NUMBER | literalString=STRING | literalBoolean='true' | literalBoolean='false')
	 */
	protected void sequence_LiteralValue(EObject context, LiteralValue semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (uri=NewModelNS imports+=Import* (triples+=Triple | triples+=Group)*)
	 */
	protected void sequence_Model(EObject context, Model semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (baseUri=STRING prefix=ID?)
	 */
	protected void sequence_NewModelNS(EObject context, NewModelNS semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     (ref=ColumnName addlcols+='_'? row='()'?)
	 */
	protected void sequence_Ref(EObject context, Ref semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
	
	
	/**
	 * Constraint:
	 *     ((subj=Ref | subj=[ResourceName|ID]) pred=[ResourceName|ID] (objval=Ref | objval=[ResourceName|ID] | objval=LiteralValue))
	 */
	protected void sequence_Triple(EObject context, Triple semanticObject) {
		genericSequencer.createSequence(context, semanticObject);
	}
}
