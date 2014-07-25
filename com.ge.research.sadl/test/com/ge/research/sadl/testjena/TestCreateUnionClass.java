package com.ge.research.sadl.testjena;

import static org.junit.Assert.*;

import org.junit.Test;

import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFList;
import com.hp.hpl.jena.rdf.model.RDFNode;

public class TestCreateUnionClass {
	
	public static String NS = "http://example.org/example3/";

	@Test
	public void test() {
		OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		
		// Create the Classes : animal, plant, sheep, grass, vegetarian;
		OntClass animal = model.createClass(NS + "Animal");
		OntClass plant = model.createClass(NS + "Plant");
		OntClass sheep = model.createClass(NS + "Sheep");
		OntClass grass = model.createClass(NS + "Grass");
		OntClass vegetarian = model.createClass(NS + "Vegetarian");
		
		// Set sheep as subClass of animal, set grass as subClass of plant;
		animal.addSubClass(sheep);
		plant.addSubClass(grass);
		
		// Create the object property eat and part_of (there domain and range are owl:Thing); 
		ObjectProperty eat = model.createObjectProperty(NS + "eat");
		ObjectProperty partOf = model.createObjectProperty(NS + "partOf");
		
		// Create Restriction : eatAllGrass, set sheep as its subclass;
		AllValuesFromRestriction avr = model.createAllValuesFromRestriction(null, eat, grass);
		avr.addSubClass(sheep);
		
		// Create Restriction : partofSomePlant; partofSomeAnimal;
		SomeValuesFromRestriction plantPart = model.createSomeValuesFromRestriction(null, partOf, plant);
		SomeValuesFromRestriction animalPart = model.createSomeValuesFromRestriction(null, partOf, animal);
		
		// Create the Union Class meat : (animal, partofSomeAnimal);
		RDFNode[] nodes1 = {animal, animalPart};
		RDFList meatList = model.createList(nodes1);
		UnionClass meat = model.createUnionClass(null, meatList);
		
		// Create the Union Class vegetable : (plant, partofSomePlant):
		RDFNode[] nodes2 = {plant, plantPart};
		RDFList vegetableList = model.createList(nodes2);
		UnionClass vegetable = model.createUnionClass(null, vegetableList);

		model.write(System.out, "RDF/XML-ABBREV");
		model.write(System.out, "N3");
	}

}
