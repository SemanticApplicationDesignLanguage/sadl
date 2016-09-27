package com.ge.research.sadl.visualize;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.processing.SadlModelProcessor;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.CardinalityRestriction;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.MaxCardinalityRestriction;
import com.hp.hpl.jena.ontology.MinCardinalityRestriction;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.OntResource;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.Syntax;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

public class GraphGenerator {
	private static final Logger logger = LoggerFactory.getLogger(GraphGenerator.class);
	OntModel model = null;
	ConceptName anchor = null;
	
	public enum Orientation {TD, LR}
	
	public class GraphSegment {
		Object subject;
		Object predicate;
		Object object;
		private boolean objectIsList = false;
		
		public GraphSegment(Object s, Object p, Object o) {
			subject = s;
			predicate = p;
			object = o;
		}
		
		public GraphSegment(Object s, Object p, Object o, boolean objIsList) {
			subject = s;
			predicate = p;
			object = o;
			setObjectIsList(objIsList);
		}

		public boolean equals(Object otherSeg) {
			if (otherSeg instanceof GraphSegment) {
				if (otherSeg != null) {
					if (subject != null && ((GraphSegment)otherSeg).subject != null && 
							predicate != null && ((GraphSegment)otherSeg).predicate != null && 
							object != null && ((GraphSegment)otherSeg).object != null) {
						if (subject.equals(((GraphSegment)otherSeg).subject) && predicate.equals(((GraphSegment)otherSeg).predicate) && 
								object.equals(((GraphSegment)otherSeg).object) && ((GraphSegment)otherSeg).isObjectIsList() == isObjectIsList()) {
							return true;
						}
					}
				}
			}
			return false;
		}
		
		String subjectToString() {
			return stringForm(subject);
		}
		
		String predicateToString() {
			return stringForm(predicate);
		}
		
		String objectToString() {
			if (!isObjectIsList()) {
				return stringForm(object);
			}
			else {
				return stringForm(object) + " List";
			}
		}
		
		String stringForm(Object obj) {
			if (obj instanceof OntClass) {
				OntClass ontcls = (OntClass) obj;
				if (ontcls.isUnionClass()) {
					UnionClass ucls = ontcls.as(UnionClass.class);
					try {
						StringBuilder sb = new StringBuilder();
						boolean first = true;
						ExtendedIterator<? extends OntClass> eitr = ucls.listOperands();
						while (eitr.hasNext()) {
							OntClass uclsmember = eitr.next();
							if (!first) {
								sb.append(" or ");
							}
							sb.append(stringForm(uclsmember));
							first = false;
						}
						return sb.toString();
					}
					catch (Exception e) {
						logger.error("Unexpected error; apparent Union Class does not return operands: " + e.getMessage());
						e.printStackTrace();
					}
				}
				else if (ontcls.isIntersectionClass()) {
					IntersectionClass ucls = ontcls.as(IntersectionClass.class);
					try {
						StringBuilder sb = new StringBuilder();
						boolean first = true;
						ExtendedIterator<? extends OntClass> eitr = ucls.listOperands();
						while (eitr.hasNext()) {
							OntClass uclsmember = eitr.next();
							if (!first) {
								sb.append(" and ");
							}
							sb.append(stringForm(uclsmember));
							first = false;
						}
						return sb.toString();
					}
					catch (Exception e) {
						logger.error("Unexpected error; apparent Interection Class does not return operands: " + e.getMessage());
						e.printStackTrace();
					}
				}
				else if (ontcls.isURIResource()) {
					return ontcls.getLocalName();
				}
				else if (ontcls.isRestriction()) {
					return restrictionToString(ontcls);
				}
				else {
					return ontcls.toString();
				}
			}
			else if (obj instanceof Resource) {
				if (((Resource)obj).isURIResource()) {
					return ((Resource) obj).getLocalName();
				}
				else {
					return obj.toString();
				}
			}
			else if (obj instanceof Literal) {
				Object objVal = ((Literal)obj).getValue();
				if (objVal instanceof Integer || objVal instanceof Long) {
					return objVal.toString() + " ";
				}
				else if (objVal instanceof Number) {
					return objVal.toString();
				}
				else  {
					String val = objVal.toString().trim();
					if (val.startsWith("\"") && val.endsWith("\"")) {
						// string is already quoted
						return objVal.toString();
					}
					else {
						return "\"" + objVal.toString() + "\"";
					}
				}
			}
			else if (obj instanceof ConceptName) {
				return ((ConceptName)obj).getName();
			}
			else {
				return obj.toString();
			}
			return null;
		}

		private String restrictionToString(OntClass ontcls) {
			if (ontcls.as(Restriction.class).isSomeValuesFromRestriction()) {
				StringBuilder sb = new StringBuilder("some values of ");
				SomeValuesFromRestriction svfr = ontcls.as(SomeValuesFromRestriction.class);
				OntProperty ontprop = svfr.getOnProperty();
				sb.append(ontprop.getLocalName());
				sb.append(" from ");
				Resource svfcls = svfr.getSomeValuesFrom();
				if (svfcls.isURIResource()) {
					sb.append(svfcls.getLocalName());
				}
				else {
					sb.append("(");
					sb.append(stringForm(svfcls));
					sb.append(")");
				}
				return sb.toString();
			}
			else if (ontcls.as(Restriction.class).isAllValuesFromRestriction()) {
				StringBuilder sb = new StringBuilder("all values of ");
				AllValuesFromRestriction svfr = ontcls.as(AllValuesFromRestriction.class);
				OntProperty ontprop = svfr.getOnProperty();
				sb.append(ontprop.getLocalName());
				sb.append(" from ");
				Resource svfcls = svfr.getAllValuesFrom();
				if (svfcls.isURIResource()) {
					sb.append(svfcls.getLocalName());
				}
				else {
					sb.append("(");
					sb.append(stringForm(svfcls));
					sb.append(")");
				}
				return sb.toString();
			}
			else if (ontcls.as(Restriction.class).isHasValueRestriction()) {
				StringBuilder sb = new StringBuilder("value of ");
				SomeValuesFromRestriction svfr = ontcls.as(SomeValuesFromRestriction.class);
				OntProperty ontprop = svfr.getOnProperty();
				sb.append(ontprop.getLocalName());
				sb.append(" is ");
				Resource svfcls = svfr.getSomeValuesFrom();
				if (svfcls.isURIResource()) {
					sb.append(svfcls.getLocalName());
				}
				else {
					sb.append("(");
					sb.append(stringForm(svfcls));
					sb.append(")");
				}
				return sb.toString();
			}
			else if (ontcls.as(Restriction.class).isMinCardinalityRestriction()) {
				StringBuilder sb = new StringBuilder();
				MinCardinalityRestriction svfr = ontcls.as(MinCardinalityRestriction.class);
				OntProperty ontprop = svfr.getOnProperty();
				sb.append(ontprop.getLocalName());
				sb.append(" has at least ");
				int card = svfr.getMinCardinality();
				sb.append(card);
				if (card > 1) {
					sb.append(" values");
				}
				else {
					sb.append(" value");
				}
				return sb.toString();
			}
			else if (ontcls.as(Restriction.class).isMaxCardinalityRestriction()) {
				StringBuilder sb = new StringBuilder();
				MaxCardinalityRestriction svfr = ontcls.as(MaxCardinalityRestriction.class);
				OntProperty ontprop = svfr.getOnProperty();
				sb.append(ontprop.getLocalName());
				sb.append(" has at most ");
				int card = svfr.getMaxCardinality();
				sb.append(card);
				if (card > 1) {
					sb.append(" values");
				}
				else {
					sb.append(" value");
				}
				return sb.toString();
			}
			else if (ontcls.as(Restriction.class).isCardinalityRestriction()) {
				StringBuilder sb = new StringBuilder();
				CardinalityRestriction svfr = ontcls.as(CardinalityRestriction.class);
				OntProperty ontprop = svfr.getOnProperty();
				sb.append(ontprop.getLocalName());
				sb.append(" has exactly ");
				int card = svfr.getCardinality();
				sb.append(card);
				if (card > 1) {
					sb.append(" values");
				}
				else {
					sb.append(" value");
				}
				return sb.toString();
			}
			else if (ontcls.isRestriction()) {
				if (ontcls.hasProperty(OWL2.onClass)) {
					OntClass oncls = ontcls.getPropertyValue(OWL2.onClass).as(OntClass.class);
					OntProperty onprop = ontcls.getPropertyValue(OWL.onProperty).as(OntProperty.class);
					if (ontcls.hasProperty(OWL2.maxQualifiedCardinality)) {
						StringBuilder sb = new StringBuilder(onprop.getLocalName());
						sb.append(" has at most ");
						int card = ontcls.getPropertyValue(OWL2.maxQualifiedCardinality).asLiteral().getInt();
						sb.append(card);
						if (card > 1) {
							sb.append(" values");
						}
						else {
							sb.append(" value");
						}
						sb.append(" of type ");
						sb.append(stringForm(oncls));
						return sb.toString();
					}
					else if (ontcls.hasProperty(OWL2.minQualifiedCardinality)) {
						StringBuilder sb = new StringBuilder(onprop.getLocalName());
						sb.append(" has at least ");
						int card = ontcls.getPropertyValue(OWL2.minQualifiedCardinality).asLiteral().getInt();
						sb.append(card);
						if (card > 1) {
							sb.append(" values");
						}
						else {
							sb.append(" value");
						}
						sb.append(" of type ");
						sb.append(stringForm(oncls));
						return sb.toString();
					}
					else if (ontcls.hasProperty(OWL2.qualifiedCardinality)) {
						StringBuilder sb = new StringBuilder(onprop.getLocalName());
						sb.append(" has exactly ");
						int card = ontcls.getPropertyValue(OWL2.qualifiedCardinality).asLiteral().getInt();
						sb.append(card);
						if (card > 1) {
							sb.append(" values");
						}
						else {
							sb.append(" value");
						}
						sb.append(" of type ");
						sb.append(stringForm(oncls));
						return sb.toString();
					}
				}
				System.out.println(ontcls.as(Restriction.class).toString() + " is an unidentified restriction with properties:");
				StmtIterator sitr = ontcls.listProperties();
				while (sitr.hasNext()) {
					Statement stmt = sitr.nextStatement();
					System.out.println("   " + stmt.getPredicate().toString() + " " + stmt.getObject().toString());
				}
			}
			return "Untranslated Restriction: " + ontcls.toString();
		}

		boolean isObjectIsList() {
			return objectIsList;
		}

		void setObjectIsList(boolean objectIsList) {
			this.objectIsList = objectIsList;
		}
	}
	
	public GraphGenerator(OntModel m, ConceptName startNode) {
		model = m;
		anchor = startNode;
	}
	
	public ResultSet generateClassHierarchy(int size) throws ConfigurationException {
		OntClass cls = model.getOntClass(anchor.toFQString());
		List<GraphSegment> data = new ArrayList<GraphSegment>();
		data = generateClassSubclasses(cls, data);
		data = generateClassSuperclasses(cls, data);
		ResultSet rs = convertDataToResultSet(data);
		return rs;
	}

	public ResultSet generateClassNeighborhood(int size) throws ConfigurationException {
		OntClass cls = model.getOntClass(anchor.toFQString());
		List<GraphSegment> data = new ArrayList<GraphSegment>();
		data = generateClassPropertiesWithDomain(cls, data);
		data = generateClassPropertiesWithRange(cls, data);
		return convertDataToResultSet(data);
	}

	public ResultSet generatePropertyNeighborhood(int size) throws ConfigurationException {
		OntProperty ontprop = model.getOntProperty(anchor.toFQString());

		List<GraphSegment> data = new ArrayList<GraphSegment>();
		ExtendedIterator<? extends OntResource> eitr = ontprop.listDomain();
		while (eitr.hasNext()) {
			OntResource dmn = eitr.next();
			if (dmn.canAs(OntClass.class)){
				data = generatePropertyRange(dmn.as(OntClass.class), ontprop, data);
			}
			data = generateClassPropertiesWithRange(dmn.as(OntClass.class), data);
		}
//		ExtendedIterator<? extends OntResource> eitr2 = ontprop.listRange();
//		while (eitr2.hasNext()) {
//			OntResource rng = eitr2.next();
//			if (ontprop.as(OntProperty.class).isObjectProperty()) {
//				data = generateClassPropertiesWithDomain(rng.as(OntClass.class), data);
//			}
//		}
		return convertDataToResultSet(data);
	}

	public ResultSet generateIndividualNeighborhood(int size) throws ConfigurationException {
		Individual inst = model.getIndividual(anchor.toFQString());

		List<GraphSegment> data = new ArrayList<GraphSegment>();
		data = generateIndividualNeighborhood(inst, data);
		return convertDataToResultSet(data);
	}

	private List<GraphSegment> generateIndividualNeighborhood(Individual inst,
			List<GraphSegment> data) {
		// as object
		StmtIterator sitr = model.listStatements(null, null, inst);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Property prop = stmt.getPredicate();
			if (prop.equals(OWL.hasValue)) {
				continue;
			}
			Resource subj = stmt.getSubject();
			GraphSegment sg = new GraphSegment(subj, prop, stmt.getObject());
			if (!stmt.getPredicate().getNameSpace().equals(RDF.getURI()) && !data.contains(sg)) {
				data.add(sg);
				if (subj.canAs(Individual.class)) {
					data = generateIndividualNeighborhood(subj.as(Individual.class), data);
				}
			}
		}
		// as subject
		sitr = inst.listProperties();
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Property prop = stmt.getPredicate();
			RDFNode obj = stmt.getObject();
			GraphSegment sg = new GraphSegment(stmt.getSubject(), prop, obj);
			if (!data.contains(sg)) {
				data.add(sg);
				if (!stmt.getPredicate().equals(RDF.type) && obj.canAs(Individual.class)) {
					data = generateIndividualNeighborhood(obj.as(Individual.class), data);
				}
			}
		}
		return data;
	}

	private List<GraphSegment> generateClassPropertiesWithRange(OntClass cls,
			List<GraphSegment> data) {
		StmtIterator sitr = model.listStatements(null, RDFS.range, cls);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Resource prop = stmt.getSubject();
			if (prop.canAs(OntProperty.class)) {
				OntProperty ontprop = prop.as(OntProperty.class);
				ExtendedIterator<? extends OntResource> eitr = ontprop.listDomain();
				while (eitr.hasNext()) {
					OntResource dmn = eitr.next();
					GraphSegment sg = new GraphSegment(dmn, prop, cls);
					if (!data.contains(sg)) {
						data.add(sg);
						data = generateClassPropertiesWithRange(dmn.as(OntClass.class), data);
					}
				}
			}
		}
		return data;
	}

	private List<GraphSegment> generateClassPropertiesWithDomain(OntClass cls,
			List<GraphSegment> data) {
		StmtIterator sitr = model.listStatements(null, RDFS.domain, cls);
		while (sitr.hasNext()) {
			Statement stmt = sitr.nextStatement();
			Resource prop = stmt.getSubject();
			if (prop.canAs(OntProperty.class)) {
				data = generatePropertyRange(cls, prop, data);
			}
		}
		// now look for unions? or intersections containing the cls
		String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
		qstr += "select ?prop where {?prop rdfs:domain/(owl:unionOf/rdf:rest*/rdf:first)? <" + cls.getURI() + ">}";
		QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), model);;		
		com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
		while (results.hasNext()) {
			QuerySolution soln = results.next();
			RDFNode prop = soln.get("?prop");
			if (prop.canAs(OntProperty.class)) {
				data = generatePropertyRange(cls, prop.as(OntProperty.class), data);
			}
		}
		return data;
	}

	private List<GraphSegment> generatePropertyRange(OntClass cls,
			Resource prop, List<GraphSegment> data) {
		boolean isList = false;
		Statement stmt = prop.getProperty(model.getAnnotationProperty(SadlModelProcessor.LIST_RANGE_ANNOTATION_PROPERTY));
		if (stmt != null) {
			RDFNode obj = stmt.getObject();
			if (obj.isLiteral()) {
				Object lit = obj.asLiteral().getValue();
				if (lit != null && lit.toString().equals("LIST")) {
					isList = true;
				}
			}
		}
		ExtendedIterator<? extends OntResource> eitr = prop.as(OntProperty.class).listRange();
		while (eitr.hasNext()) {
			OntResource rng = eitr.next();
			if (isList && rng.canAs(OntClass.class)) {
				Resource listClass = model.getResource(SadlModelProcessor.SADL_LIST_MODEL_LIST_URI);
				if (listClass == null || rng.as(OntClass.class).hasSuperClass(listClass)) {
					StmtIterator stmtitr = model.listStatements(rng, RDFS.subClassOf, (RDFNode)null);
					while (stmtitr.hasNext()) {
//					ExtendedIterator<OntClass> scitr = rng.as(OntClass.class).listSuperClasses(true);
//					while (scitr.hasNext()) {
						Statement supclsstmt = stmtitr.nextStatement();
						RDFNode supclsnode = supclsstmt.getObject();
						if (supclsnode.canAs(OntClass.class)){ 
							OntClass subcls = supclsnode.as(OntClass.class);  // scitr.next();
							if (subcls.hasProperty(OWL.onProperty, model.getProperty(SadlModelProcessor.SADL_LIST_MODEL_FIRST_URI))) {
								Statement avf = subcls.getProperty(OWL.allValuesFrom);
								if (avf != null) {
									RDFNode listtype = avf.getObject();
									if (listtype.canAs(OntResource.class)){
										rng = listtype.as(OntResource.class);
									}
								}
								stmtitr.close();
								break;
							}
						}
					}
				}
			}
			GraphSegment sg = isList ? new GraphSegment(cls, prop, rng, isList) : new GraphSegment(cls, prop, rng);
			if (!data.contains(sg)) {
				data.add(sg);
				if (prop.as(OntProperty.class).isObjectProperty()) {
					data = generateClassPropertiesWithDomain(rng.as(OntClass.class), data);
				}
			}
		}
		return data;
	}

	private List<GraphSegment> generateClassSuperclasses(OntClass cls, List<GraphSegment> data) {
		ExtendedIterator<OntClass> eitr = cls.listSuperClasses(true);
		while (eitr.hasNext()) {
			Resource scr = eitr.next();

//		StmtIterator sitr = getJenaModel().listStatements(cls, RDFS.subClassOf, (Resource)null);
//		while (sitr.hasNext()) {
//			Resource scr = sitr.next().getSubject();
			
			if (scr != null && !scr.equals(cls) && scr.canAs(OntClass.class)) {
				OntClass supercls = scr.as(OntClass.class);
				GraphSegment sg;
				if (supercls.isRestriction()) {
					sg = new GraphSegment(supercls, "restricts", cls);
				}
				else {
					sg = new GraphSegment(supercls, "subClass", cls);
				}
				if (!data.contains(sg)) {
					data.add(sg);
				}
				data = generateClassSuperclasses(supercls, data);
			}
		}
		if (cls.isUnionClass()) {
			// if all of the members of a Union class are subclasses of a common super class, then that common super class is a super class of the Union class
			ExtendedIterator<? extends OntClass> eunitr = cls.asUnionClass().listOperands();
			List<OntClass> commonSupers = new ArrayList<OntClass>();
			int idx = 0;
			while (eunitr.hasNext()) {
				OntClass member = eunitr.next();
				ExtendedIterator<OntClass> sclsitr = member.listSuperClasses(false);
				List<OntClass> otherSupers = new ArrayList<OntClass>();
				while (sclsitr.hasNext()) {
					if (idx == 0) {
						// first pass add all members to list
						commonSupers.add(sclsitr.next());
					}
					else {
						// subsequent pass 
						otherSupers.add(sclsitr.next());
					}
				}
				if (idx > 0) {
					// remove any members from commonSupers that are not in otherSupers
					for (int i = commonSupers.size() - 1; i >= 0; i--) {
						OntClass ccls = commonSupers.get(i);
						if (!otherSupers.contains(ccls)) {
							commonSupers.remove(ccls);
						}
					}
				}
				idx++;
			}
			if (commonSupers.size() > 0) {
				for (int i = 0; i < commonSupers.size(); i++) {
					OntClass cscls = commonSupers.get(i);
					GraphSegment sg = new GraphSegment(cscls, "subClass", cls);
					if (!data.contains(sg)) {
						data.add(sg);
					}
					data = generateClassSuperclasses(cscls, data);
				}
			}
		}
		else if (cls.isIntersectionClass()) {
			// each member class of an Intersection class is a super class of the Intersection class
			ExtendedIterator<? extends OntClass> einteritr = cls.asIntersectionClass().listOperands();
			while (einteritr.hasNext()) {
				OntClass member  = einteritr.next();
				GraphSegment sg = new GraphSegment(member, "subClass", cls);
				if (!data.contains(sg)) {
					data.add(sg);
					data = generateClassSuperclasses(member, data);
				}
			}
		}
		return data;
	}
	private List<GraphSegment> generateClassSubclasses(OntClass cls, List<GraphSegment> data) {
		ExtendedIterator<OntClass> eitr = cls.listSubClasses(true);
		while (eitr.hasNext()) {
			OntClass subcls = eitr.next();
			GraphSegment sg;
			if (subcls.isRestriction()) {
				sg = new GraphSegment(cls, "restricts", subcls);
			}
			else {
				sg = new GraphSegment(cls, "subClass", subcls);
			}
			if (!data.contains(sg)) {
				data.add(sg);
				data = generateClassSubclasses(subcls, data);
			}
		}
		
		// if this class is part of an intersection, then the superclass of the intersection is a subclass of this class
		if (cls.isURIResource()) {
			String qstr = "prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> prefix owl:   <http://www.w3.org/2002/07/owl#> prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
	//		qstr += "select ?subclass ?superclass where {?subclass rdfs:subClassOf/(owl:intersectionOf/rdf:rest*/rdf:first)? ?superclass}";
			qstr += "select ?subclass ?superclass where {?subclass rdfs:subClassOf/(owl:intersectionOf/rdf:rest*/rdf:first)? <" + cls.getURI() + ">}";
			QueryExecution qexec = QueryExecutionFactory.create(QueryFactory.create(qstr, Syntax.syntaxARQ), model);;		
			com.hp.hpl.jena.query.ResultSet results = qexec.execSelect();
			while (results.hasNext()) {
				QuerySolution soln = results.next();
				RDFNode sub = soln.get("?subclass");
				if (sub.canAs(OntClass.class)) {
					OntClass subcls = sub.as(OntClass.class);
					GraphSegment sg = new GraphSegment(cls, "subClass", subcls);
					if (!data.contains(sg)) {
						data.add(sg);
						data = generateClassSubclasses(subcls, data);
					}	
				}
			}
		}
//		StmtIterator sitr = model.listStatements(null, OWL.intersectionOf, cls);
//		while (sitr.hasNext()) {
//			OntClass intersectCls = sitr.nextStatement().getSubject().as(OntClass.class);
//			StmtIterator sitr2 = model.listStatements(null, RDFS.subClassOf, intersectCls);
//			while (sitr2.hasNext()) {
//				OntClass subcls = sitr2.nextStatement().getSubject().as(OntClass.class);
//				GraphSegment sg = new GraphSegment(cls, "subClass", subcls);
//				if (!data.contains(sg)) {
//					data.add(sg);
//					data = generateClassSubclasses(subcls, data);
//				}	
//			}
//		}
		return data;
	}

	private ResultSet convertDataToResultSet(List<GraphSegment> data) {
		Object array[][] = new Object[data.size()][3];
		boolean dataFound = false;
		for(int i = 0; i < data.size(); i++) {
			String s = data.get(i).subjectToString();
			String p = data.get(i).predicateToString();
			String o = data.get(i).objectToString();
			array[i][0] = s;
			array[i][1] = p;
			array[i][2] = o;
			dataFound = true;
		}
		if (dataFound) {
			ResultSet rs = new ResultSet(array);
			return rs;
		}
		return null;
	}

}
