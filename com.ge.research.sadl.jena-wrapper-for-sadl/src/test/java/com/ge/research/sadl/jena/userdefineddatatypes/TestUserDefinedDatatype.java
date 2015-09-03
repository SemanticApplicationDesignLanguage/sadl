package com.ge.research.sadl.jena.userdefineddatatypes;

import static org.junit.Assert.*;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.net.URL;
import java.util.Iterator;
import java.util.List;

import org.apache.xerces.impl.dv.XSSimpleType;
import org.apache.xerces.impl.dv.xs.XSSimpleTypeDecl;
import org.apache.xerces.xs.XSAnnotation;
import org.apache.xerces.xs.XSNamespaceItem;
import org.junit.Ignore;
import org.junit.Test;

import com.hp.hpl.jena.datatypes.DatatypeFormatException;
import com.hp.hpl.jena.datatypes.RDFDatatype;
import com.hp.hpl.jena.datatypes.TypeMapper;
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype;
//import com.hp.hpl.jena.datatypes.xsd.XSDDatatype.XSDGenericType;
import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.reasoner.ValidityReport;
import com.hp.hpl.jena.reasoner.ValidityReport.Report;

public class TestUserDefinedDatatype {
	

	@Test
	@Ignore
	public void test() throws DatatypeFormatException, FileNotFoundException {
		URL dataModelsFolder = ClassLoader.getSystemResource("testing");
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
        XSDDatatype.loadUserDefined(uri, new FileReader(filename2), null, tm);

        types = tm.listTypes();
        while (types.hasNext()) {
        	RDFDatatype dt = types.next();
        	System.out.println(dt.getURI());
        }

        System.out.println("\n\nLoading user-defined '" + filename3 + "':");
        XSDDatatype.loadUserDefined(uri, new FileReader(filename3), null, tm);

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
