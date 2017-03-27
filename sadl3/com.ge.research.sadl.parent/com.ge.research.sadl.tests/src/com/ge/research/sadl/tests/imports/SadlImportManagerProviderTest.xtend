/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.tests.imports

import com.ge.research.sadl.jena.JenaBasedSadlImportProcessor
import com.ge.research.sadl.jena.JenaBasedSadlModelProcessor
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext
import com.ge.research.sadl.processing.ValidationAcceptor
import com.ge.research.sadl.sADL.SadlModel
import com.ge.research.sadl.tests.SADLInjectorProvider
import com.google.inject.Inject
import com.google.inject.Provider
import com.hp.hpl.jena.ontology.OntModel
import com.hp.hpl.jena.query.QueryExecutionFactory
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.util.ArrayList
import java.util.List
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.eclipse.xtext.preferences.IPreferenceValuesProvider
import org.eclipse.xtext.util.CancelIndicator
import org.eclipse.xtext.validation.Issue
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import org.eclipse.xtext.validation.CheckMode

@RunWith(XtextRunner)
@InjectWith(SADLInjectorProvider)
class SadlImportManagerProviderTest {
	
	@Inject ParseHelper<SadlModel> parser
	@Inject ValidationTestHelper validationTestHelper
	@Inject Provider<JenaBasedSadlModelProcessor> smProcessorProvider
	@Inject Provider<JenaBasedSadlImportProcessor> impProcessorProvider
	@Inject IPreferenceValuesProvider preferenceProvider
	
/* Tests that should generate validation errors */	
	@Ignore
	@Test def void testDatatypeImports1() {
		'''
			<rdf:RDF
			    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
			    xmlns:owl="http://www.w3.org/2002/07/owl#"
			    xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
			    xmlns:latitude="http://sadl.org/TestRequrements/Latitude#"
			    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
			  xml:base="http://sadl.org/TestRequrements/Latitude">
			  <owl:Ontology rdf:about="">
			    <owl:versionInfo>$Revision: 1.1 $ Last modified on   $Date: 2015/01/27 22:39:41 $</owl:versionInfo>
			    <rdfs:comment xml:lang="en">This ontology was created from a SADL file 'Latitude.sadl' and should not be directly edited.</rdfs:comment>
			  </owl:Ontology>
			  <rdfs:Datatype rdf:ID="Longitude">
			    <owl:equivalentClass>
			      <rdfs:Datatype>
			        <owl:withRestrictions rdf:parseType="Collection">
				     <rdf:Description>
			            <xsd:maxExclusive>180</xsd:maxExclusive>
			            <xsd:minExclusive>-180</xsd:minExclusive>
				     </rdf:Description>
			        </owl:withRestrictions>
			        <owl:onDatatype rdf:resource="http://www.w3.org/2001/XMLSchema#float"/>
			      </rdfs:Datatype>
			    </owl:equivalentClass>
			  </rdfs:Datatype>
			  <rdfs:Datatype rdf:ID="Latitude">
			    <owl:equivalentClass>
			      <rdfs:Datatype>
			        <owl:withRestrictions rdf:parseType="Collection">
				     <rdf:Description>
			            <xsd:maxInclusive>90</xsd:maxInclusive>
			            <xsd:minInclusive>-90</xsd:minInclusive>
				     </rdf:Description>
			        </owl:withRestrictions>
			        <owl:onDatatype rdf:resource="http://www.w3.org/2001/XMLSchema#float"/>
			      </rdfs:Datatype>
			    </owl:equivalentClass>
			  </rdfs:Datatype>
			  <owl:Class rdf:ID="Position"/>
			  <owl:DatatypeProperty rdf:ID="description">
			    <rdfs:domain rdf:resource="#Position"/>
			    <rdfs:range rdf:resource="http://www.w3.org/2001/XMLSchema#string"/>
			  </owl:DatatypeProperty>
			  <owl:DatatypeProperty rdf:ID="longitude">
			    <rdfs:domain rdf:resource="#Position"/>
			    <rdfs:range rdf:resource="#Longitude"/>
			  </owl:DatatypeProperty>
			  <owl:DatatypeProperty rdf:ID="latitude">
			    <rdfs:domain rdf:resource="#Position"/>
			    <rdfs:range rdf:resource="#Latitude"/>
			  </owl:DatatypeProperty>
			</rdf:RDF>
		'''.assertValidatesTo [ jenaModel, issues |
			assertNotNull(jenaModel)
			assertTrue(issues.size == 0)
		]
	}
	

//	@Test def void my<younameit>Case() {
//		'''
//			// model goes here
//		'''.assertValidatesTo [ jenaModel, issues |
//			// expectations go here
//			assertNotNull(jenaModel)
//			assertTrue(issues.size == 0)
//			var found = false
//			// look for something specific to the model; if found set found true
//
//			if (!found) {
//				jenaModel.write(System.out, "N3")				
//			}
//			assertTrue(found);
//		]
//	}
	
	protected def boolean queryResultContains(OntModel m, String q, String r) {
		var qe = QueryExecutionFactory.create(q, m)
		var results =  qe.execSelect()
		var vars = results.resultVars
    	var resultsList = new ArrayList<String>()
    	while (results.hasNext()) {
    		var result = results.next()
    		var sb = new StringBuffer();
    		var cntr = 0
    		for (var c = 0; c < vars.size(); c++) {
    			if (cntr++ > 0) {
    				sb.append(" ")
    			}
    			sb.append(result.get(vars.get(c)))
    		}
    		resultsList.add(sb.toString())
    	}
    	if (resultsList.contains(r)) {
    		return true
    	}
    	System.out.println("Query result does not contain '" + r + "':")
    	var itr = resultsList.iterator()
    	if (itr.hasNext()) {
    		while (itr.hasNext()) {
    			System.out.println("   " + itr.next().toString())
    		}
    	}
    	else {
    		System.out.println("    Query returned no results");
    	}
    	return false
	}
	
	protected def Resource assertValidatesTo(CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val impprocessor = impProcessorProvider.get
		val Object[] results = impprocessor.onImport(code.toString);
//		val InputStream is = results.get(0) as InputStream
//		val String sadl = streamToString(is)
		val String sadl = results.get(0) as String
		System.out.println(sadl)
		val model = parser.parse(sadl)
		validationTestHelper.assertNoErrors(model)
		val smprocessor = smProcessorProvider.get
		val List<Issue> issues= newArrayList
		smprocessor.onValidate(model.eResource, new ValidationAcceptor([issues += it]), CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(model.eResource)))
		assertions.apply(smprocessor.theJenaModel, issues)
		return model.eResource
	}

	protected def Resource assertValidatesTo(ResourceSet resourceSet, CharSequence code, (OntModel, List<Issue>)=>void assertions) {
		val model = parser.parse(code, resourceSet);
		val xtextIssues = validationTestHelper.validate(model);
		val processor = smProcessorProvider.get
		val List<Issue> issues= new ArrayList(xtextIssues);
		processor.onValidate(model.eResource, new ValidationAcceptor([issues += it]),  CheckMode.FAST_ONLY, new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(model.eResource)))
		assertions.apply(processor.theJenaModel, issues)
		return model.eResource
	}
	
	protected def String streamToString(InputStream in) throws IOException {
	  val StringBuilder out = new StringBuilder();
	  val BufferedReader br = new BufferedReader(new InputStreamReader(in));
	  var String line = br.readLine()
	  while (line !== null) {
	  	out.append(line)
	  	out.append("\n")
	  	line = br.readLine()
	  }
	  br.close();
	  return out.toString();
	}
}
		