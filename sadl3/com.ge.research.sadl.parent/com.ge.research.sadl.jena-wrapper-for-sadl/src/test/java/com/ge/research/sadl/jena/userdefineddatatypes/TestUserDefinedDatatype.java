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
package com.ge.research.sadl.jena.userdefineddatatypes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.FileNotFoundException;
import java.net.URL;
import java.util.Iterator;

import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.datatypes.TypeMapper;
//import org.apache.jena.datatypes.xsd.XSDDatatype.XSDGenericType;
import org.apache.jena.graph.Triple;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.reasoner.ValidityReport;
import org.apache.jena.reasoner.ValidityReport.Report;
import org.apache.xerces.impl.dv.XSSimpleType;
import org.apache.xerces.impl.dv.xs.XSSimpleTypeDecl;
import org.junit.Ignore;
import org.junit.Test;

import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.AmbiguousNameException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;

public class TestUserDefinedDatatype {
	

	@Test
	public void test_01() throws ConfigurationException, ReasonerNotFoundException, QueryParseException, QueryCancelledException, InvalidNameException, AmbiguousNameException {
		URL dataModelsFolder = ClassLoader.getSystemResource("TestModels");
		String kbid = dataModelsFolder.getFile() + "/GH-620";
		String mfp = kbid + "/OwlModels";
		String modelName = "http://sadl.org/UserDefinedDataTypes.sadl";
		OntModel om = ModelFactory.createOntologyModel();
		om.read(SadlUtils.FILE_ABS_URL_PREFIX + mfp + "/UserDefinedDataTypes.owl");
		om.write(System.out);
		Resource sldt = om.getResource(modelName + "#SL");
	    RDFDatatype rdfdtype = TypeMapper.getInstance().getTypeByName(sldt.getURI());
		assertTrue(rdfdtype instanceof RDFDatatype);
		Literal lit1 = om.createTypedLiteral(4, rdfdtype);
		System.out.println(lit1.getValue().toString());
		Literal lit2 = om.createTypedLiteral(6, rdfdtype);
		System.out.println(lit2.getValue().toString());
//		OntClass myclass = om.getOntClass(modelName + "#MyClass");
//		Individual imc = om.createIndividual(modelName + "#mc1", myclass);
//		OntProperty p1 = om.getOntProperty(modelName + "#p1");
//		RDFNode lit5 = om.createTypedLiteral(v)
//		om.add(imc, p1, 5);
//		om.add(imc, )
//		om.add(statements)
	}

	@Test
	@Ignore
	public void test() throws DatatypeFormatException, FileNotFoundException {
		URL dataModelsFolder = ClassLoader.getSystemResource("TestModels");
//		URL dataModelsFolder = ClassLoader.getSystemResource("testing");
        String uri = "http://www.daml.org/2001/03/daml+oil-ex-dt";
        String filename1 = dataModelsFolder.getFile() + "/xsd/daml+oil-ex-dt.xsd";
        String filename2 = dataModelsFolder.getFile() + "/xsd/over12.xsd";
        String filename3 = dataModelsFolder.getFile() + "/xsd/enumeratedheight.xsd";
        TypeMapper tm = TypeMapper.getInstance();

        Iterator<RDFDatatype> types = tm.listTypes();
        while (types.hasNext()) {
        	RDFDatatype dt = types.next();
        	System.out.println(dt.getURI());
        }

        System.out.println("\n\nLoading user-defined '" + filename2 + "':");
//        XSDDatatype.loadUserDefined(uri, new FileReader(filename2), null, tm);

        types = tm.listTypes();
        while (types.hasNext()) {
        	RDFDatatype dt = types.next();
        	System.out.println(dt.getURI());
        }

        System.out.println("\n\nLoading user-defined '" + filename3 + "':");
//        XSDDatatype.loadUserDefined(uri, new FileReader(filename3), null, tm);

        types = tm.listTypes();
        while (types.hasNext()) {
        	RDFDatatype dt = types.next();
        	System.out.println(dt.getURI());
        }

        RDFDatatype over12Type = tm.getSafeTypeByName(uri + "#over12");
        Object etd = over12Type.extendedTypeDefinition();
        Object xsa = ((XSSimpleTypeDecl)etd).getMinExclusiveValue();
        
        XSSimpleType st = new XSSimpleTypeDecl();
//        ((XSSimpleTypeDecl)st).setNamespaceItem(new XSNamespaceItem());
        
//        XSDGenericType newDT = new XSDDataType.XSDGenericType(st, "http://com.ge.research/ns");
//        XSDGenericType newDT = new XSDGenericType(st, "http://com.ge.research/ns");

        doTestDatatypeRangeValidation(over12Type, OntModelSpec.OWL_MEM_MICRO_RULE_INF);
        doTestDatatypeRangeValidation(over12Type, OntModelSpec.OWL_MEM_MINI_RULE_INF);
        doTestDatatypeRangeValidation(over12Type, OntModelSpec.OWL_MEM_RULE_INF);
	}
	
	private void doTestDatatypeRangeValidation(RDFDatatype over12Type, OntModelSpec spec) {	
		OntModel ont = ModelFactory.createOntologyModel(spec);
		String NS = "http://jena.hpl.hp.com/example#";
	    Resource over12 = ont.createResource( over12Type.getURI() );
	    DatatypeProperty hasValue = ont.createDatatypeProperty(NS + "hasValue");
	    hasValue.addRange( over12 );
	    
	    ont.createResource(NS + "a").addProperty(hasValue, "15", over12Type);
	    ont.createResource(NS + "b").addProperty(hasValue, "16", over12Type);
	    ont.createResource(NS + "c").addProperty(hasValue, "10", over12Type);
	    
	    ValidityReport validity = ont.validate();
	    assertTrue (! validity.isValid()); 
	    Iterator<Report> ritr = validity.getReports();
	    while (ritr.hasNext()) {
	    	System.out.println("For spec '" + spec + "': " + ritr.next().toString());
	    }
	    ont.write(System.out);
	    
	    // Check culprit reporting
	    ValidityReport.Report report = (validity.getReports().next());
	    Triple culprit = (Triple)report.getExtension();
	    assertEquals(culprit.getSubject().getURI(), NS + "c");
	    assertEquals(culprit.getPredicate(), hasValue.asNode());
	    
//	    ont.createTypedLiteral("15", over12Type).getValue();
//	    ont.createTypedLiteral("16", over12Type).getValue();
//	    ont.createTypedLiteral("12", over12Type).getValue();
	}
}
