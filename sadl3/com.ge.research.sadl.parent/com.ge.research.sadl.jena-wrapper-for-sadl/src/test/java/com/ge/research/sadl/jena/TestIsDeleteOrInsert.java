package com.ge.research.sadl.jena;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.BeforeClass;
import org.junit.Test;

import com.ge.research.sadl.jena.reasoner.JenaReasonerPlugin;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;

public class TestIsDeleteOrInsert {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@Test
	public void test_01() throws QueryParseException, QueryCancelledException {
		String q = "prefix cg:<http://aske.ge.com/compgraphmodel#>\r\n" + 
				"prefix imp:<http://sadl.org/sadlimplicitmodel#>\r\n" + 
				"prefix sci:<http://aske.ge.com/sciknow#>\r\n" + 
				"prefix list:<http://sadl.org/sadllistmodel#>\r\n" + 
				"prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\r\n" + 
				"prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>\r\n" + 
				"\r\n" + 
				"insert \r\n" + 
				"{ ?Eq imp:genericInput ?In. ?Eq imp:genericOutput ?Out. }\r\n" + 
				"where {\r\n" + 
				"  \r\n" + 
				" {?Eq imp:returnTypes ?AL1.\r\n" + 
				"  ?AL1 list:rest*/list:first ?AO1.\r\n" + 
				"  ?AO1 imp:augmentedType ?Type1.\r\n" + 
				"  ?Type1 imp:constraints ?CL1.\r\n" + 
				"  ?CL1 rdf:rest*/rdf:first ?C1.\r\n" + 
				"  ?C1 imp:gpPredicate ?P.\r\n" + 
				"  ?P rdfs:range ?Out. }\r\n" + 
				"\r\n" + 
				"  union {\r\n" + 
				"    ?Eq imp:implicitOutput/imp:augmentedType/imp:semType ?Out.\r\n" + 
				"    filter not exists{?Eq a imp:IntializerMethod} }\r\n" + 
				"\r\n" + 
				"  union{\r\n" + 
				"   ?Eq imp:arguments ?AL2.\r\n" + 
				"   ?AL2 list:rest*/list:first ?AO2.\r\n" + 
				"   ?AO2 imp:augmentedType ?Type2.\r\n" + 
				"   ?Type2 imp:constraints ?CL2.\r\n" + 
				"   ?CL2 rdf:rest*/rdf:first ?C2.\r\n" + 
				"   ?C2 imp:gpPredicate ?P.\r\n" + 
				"   ?P rdfs:range ?In.}\r\n" + 
				"\r\n" + 
				"  union { #Explicit inputs w/o AT\r\n" + 
				"    ?Eq imp:arguments ?AL2.\r\n" + 
				"    ?AL2 list:rest*/list:first ?AO2.\r\n" + 
				"    filter not exists {?AO2 imp:augmentedType []}\r\n" + 
				"    ?AO2 imp:localDescriptorName ?In.}\r\n" + 
				"\r\n" + 
				"  union {?Eq imp:implicitInput/imp:augmentedType/imp:semType ?In.\r\n" + 
				"   filter not exists{?Eq a imp:IntializerMethod} }\r\n" + 
				"\r\n" + 
				"  union {\r\n" + 
				"   ?Eq imp:implicitOutput/imp:localDescriptorName ?Out.\r\n" + 
				"   filter not exists{?Eq a imp:IntializerMethod} }\r\n" + 
				"  union {\r\n" + 
				"   ?Eq imp:implicitInput?IO.\r\n" + 
				"   filter not exists {?IO imp:augmentedType [] }\r\n" + 
				"   ?IO imp:localDescriptorName ?In.\r\n" + 
				"   filter not exists{?Eq a imp:IntializerMethod} } \r\n" + 
				"}\r\n";

	
		JenaReasonerPlugin jrp = new JenaReasonerPlugin();
		assertTrue(jrp.isDeleteOrInsert(q));
	}

	
	@Test
	public void test_02() throws QueryParseException, QueryCancelledException {
		String q = "DELETE WHERE \r\n" + 
				"  {\r\n" + 
				"    GRAPH <urn:sparql:tests:delete:where1> \r\n" + 
				"      {\r\n" + 
				"        ?person <http://xmlns.com/foaf/0.1/givenName> 'Fred'  ; \r\n" + 
				"                                           ?property1 ?value1 . \r\n" + 
				"      }\r\n" + 
				"    GRAPH <urn:sparql:tests:delete:where2> \r\n" + 
				"      {\r\n" + 
				"        ?person ?property2 ?value2 . \r\n" + 
				"      }\r\n" + 
				"  }";

	
		JenaReasonerPlugin jrp = new JenaReasonerPlugin();
		assertTrue(jrp.isDeleteOrInsert(q));
	}
	@Test
	public void test_03() throws QueryParseException, QueryCancelledException {
		String q = 			"select distinct ?m ?pm ?ts ?ps ?ptfs where {?m <rdf:type> <ExternalEquation> . "
				+ "OPTIONAL {?m <http://sadl.org/sadlimplicitmodel#derivedFrom> ?pm} . "
				+ "OPTIONAL { ?m <expression> ?exp . ?exp <language> <Text> . ?exp <script> ?ts} . "
				+ "OPTIONAL {?m <expression> ?exp2 . ?exp2 <language> <Python> . ?exp2 <script> ?ps} . "
				+ "OPTIONAL {?m <expression> ?exp3 . ?exp3 <language> <Python-TF> . ?exp3 <script> ?ptfs}}";

	
		JenaReasonerPlugin jrp = new JenaReasonerPlugin();
		assertFalse(jrp.isDeleteOrInsert(q));
	}
}
