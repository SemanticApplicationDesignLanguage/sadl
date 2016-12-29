package com.ge.research.sadl.ui.visualize;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.ui.visualize.GraphGenerator.UriStrategy;
import com.hp.hpl.jena.ontology.AllValuesFromRestriction;
import com.hp.hpl.jena.ontology.CardinalityRestriction;
import com.hp.hpl.jena.ontology.IntersectionClass;
import com.hp.hpl.jena.ontology.MaxCardinalityRestriction;
import com.hp.hpl.jena.ontology.MinCardinalityRestriction;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntProperty;
import com.hp.hpl.jena.ontology.Restriction;
import com.hp.hpl.jena.ontology.SomeValuesFromRestriction;
import com.hp.hpl.jena.ontology.UnionClass;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;
import com.hp.hpl.jena.vocabulary.OWL;
import com.hp.hpl.jena.vocabulary.OWL2;

public class GraphSegment {
	private static final Logger logger = LoggerFactory.getLogger(GraphGenerator.class);
	private Object subject;
	private Object predicate;
	private Object object;
	private Map<String,String> headAttributes;
	private Map<String,String> edgeAttributes;
	private Map<String,String> tailAttributes;
	
	IConfigurationManagerForIDE configMgr;
	private boolean subjectIsList = false;
	private boolean objectIsList = false;
	
	public GraphSegment(Object s, Object p, Object o, Object cm) {
		setSubject(s);
		setPredicate(p);
		setObject(o);
		configMgr = (IConfigurationManagerForIDE) cm;
	}
	
	public GraphSegment(Object s, Object p, Object o, boolean objIsList, Object cm) {
		setSubject(s);
		setPredicate(p);
		setObject(o);
		configMgr = (IConfigurationManagerForIDE) cm;
		setObjectIsList(objIsList);
	}

	public boolean equals(Object otherSeg) {
		if (otherSeg instanceof GraphSegment) {
			if (otherSeg != null) {
				if (getSubject() != null && ((GraphSegment)otherSeg).getSubject() != null && 
						getPredicate() != null && ((GraphSegment)otherSeg).getPredicate() != null && 
						getObject() != null && ((GraphSegment)otherSeg).getObject() != null) {
					if (getSubject().equals(((GraphSegment)otherSeg).getSubject()) && getPredicate().equals(((GraphSegment)otherSeg).getPredicate()) && 
							getObject().equals(((GraphSegment)otherSeg).getObject()) && ((GraphSegment)otherSeg).isObjectIsList() == isObjectIsList()) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
//	String subjectToString(UriStrategy uriStrategy) {
//		return stringForm(getSubject());
//	}
//	
//	String predicateToString(UriStrategy uriStrategy) {
//		return stringForm(getPredicate());
//	}
//	
//	String objectToString(UriStrategy uriStrategy) {
//		if (!isObjectIsList()) {
//			return stringForm(getObject());
//		}
//		else {
//			return stringForm(getObject()) + " List";
//		}
//	}
	
	String stringForm(Object obj) {
		if(obj == null){
			return null;
		}
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
				return uriResourceToString(ontcls);
//				return ontcls.getLocalName();
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
				return uriResourceToString((Resource) obj);
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
			return conceptNameToString((ConceptName)obj);
		}
		else {
			return obj.toString();
		}
		return null;
	}

	private String conceptNameToString(ConceptName obj) {
		// get the prefix and if there is one generate qname
		if (obj.getPrefix() != null) {
			return obj.getPrefix() + ":" + obj.getName();
		}
		return obj.getName();
	}
	

	private String uriResourceToString(Resource rsrc) {
		if (!rsrc.isURIResource()) {
			return rsrc.toString();
		}
		String ns = rsrc.getNameSpace();
		if (ns.endsWith("#")) {
			ns = ns.substring(0, ns.length() - 1);
		}
		// get the prefix and if there is one generate qname
		String prefix = configMgr.getGlobalPrefix(ns);
		if (prefix != null) {
			return prefix + ":" + rsrc.getLocalName();
		}
		return rsrc.getLocalName();
	}
	
	public String restrictionToString(OntClass ontcls) {
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
	
	String subjectToStringNoPrefix() {
		return stringFormNoPrefix(getSubject());
	}
	
	String predicateToStringNoPrefix() {
		return stringFormNoPrefix(getPredicate());
	}
	
	String objectToStringNoPrefix() {
		if (!isObjectIsList()) {
			return stringFormNoPrefix(getObject());
		}
		else {
			return stringFormNoPrefix(getObject()) + " List";
		}
	}
	
	String stringFormNoPrefix(Object obj) {
		if(obj == null){
			return null;
		}
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
						sb.append(stringFormNoPrefix(uclsmember));
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
						sb.append(stringFormNoPrefix(uclsmember));
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
				return uriResourceToStringNoPrefix(ontcls);
//				return ontcls.getLocalName();
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
				return uriResourceToStringNoPrefix((Resource) obj);
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
			return conceptNameToStringNoPrefix((ConceptName)obj);
		}
		else {
			return obj.toString();
		}
		return null;
	}
	
	private String conceptNameToStringNoPrefix(ConceptName obj) {
		return obj.getName();
	}
	
	private String uriResourceToStringNoPrefix(Resource rsrc) {
		if (!rsrc.isURIResource()) {
			return rsrc.toString();
		}
		String ns = rsrc.getNameSpace();
		if (ns.endsWith("#")) {
			ns = ns.substring(0, ns.length() - 1);
		}

		return rsrc.getLocalName();
	}
	
	public boolean isSubjectIsList() {
		return subjectIsList;
	}

	public void setSubjectIsList(boolean subjectIsList) {
		this.subjectIsList = subjectIsList;
	}	

	public boolean isObjectIsList() {
		return objectIsList;
	}

	public void setObjectIsList(boolean objectIsList) {
		this.objectIsList = objectIsList;
	}

	public Map<String,String> getHeadAttributes() {
		return headAttributes;
	}

	public void addHeadAttribute(String key, String value) {
		if (headAttributes == null) headAttributes = new HashMap<String,String>();
		headAttributes.put(key, value);
	}

	public Map<String,String> getEdgeAttributes() {
		return edgeAttributes;
	}

	public void addEdgeAttribute(String key, String value) {
		if (edgeAttributes == null) edgeAttributes = new HashMap<String,String>();
		edgeAttributes.put(key, value);
	}

	public Map<String,String> getTailAttributes() {
		return tailAttributes;
	}

	public void addTailAttribute(String key, String value) {
		if (tailAttributes == null) tailAttributes = new HashMap<String,String>();
		tailAttributes.put(key,value);
	}

	public void implementUriStrategy(UriStrategy uriStrategy) {
		String newHead = null;
		String headTooltip = null;
		String newEdge = null;
		String edgeTooltip = null;
		String newTail = null;
		String tailTooltip = null;
		if (uriStrategy == null) {
			uriStrategy = UriStrategy.QNAME_ONLY; // default
		}
		if (uriStrategy.equals(UriStrategy.LOCALNAME_ONLY)) {
//			newHead = 
		}
		else if (uriStrategy.equals(UriStrategy.LOCALNAME_WITH_QNAME_TOOLTIP)) {
			
		}
		else if (uriStrategy.equals(UriStrategy.LOCALNAME_WITH_URI_TOOLTIP)) {
			
		}
		else if (uriStrategy.equals(UriStrategy.QNAME_ONLY)) {
			
		}
		else if (uriStrategy.equals(UriStrategy.QNAME_WITH_URI_TOOLTIP)) {
			
		}
		else if (uriStrategy.equals(UriStrategy.URI_ONLY)) {
			
		}
	}

	public Object getSubject() {
		return subject;
	}

	private void setSubject(Object subject) {
		this.subject = subject;
	}

	public Object getPredicate() {
		return predicate;
	}

	private void setPredicate(Object predicate) {
		this.predicate = predicate;
	}

	public Object getObject() {
		return object;
	}

	private void setObject(Object object) {
		this.object = object;
	}

}
