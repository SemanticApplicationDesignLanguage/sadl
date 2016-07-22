/************************************************************************
 * Copyright (c) 2007-2015 - General Electric Company, All Rights Reserved
 *
 * Project: SADL Knowledge Server
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language. 
 * 
 * The SADL Knowledge Server is a set of Java classes implementing 
 * a service interface for deploying ontology-based knowledge bases
 * for use in a client-server environment.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/09 14:06:51 $
 ***********************************************************************/

package com.ge.research.sadl.server.server;

import java.io.IOException;
import java.util.ResourceBundle;

import javax.naming.NameNotFoundException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.server.ISadlServerMD;
import com.ge.research.sadl.server.SessionNotFoundException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.utils.SadlUtils;

public class SadlServerMDImpl extends SadlServerPEImpl implements ISadlServerMD {
    protected static final Logger logger = LoggerFactory.getLogger(SadlServerMDImpl.class);	
	
	private String outputFormat = "RDF/XML-ABBREV";		// default
	private String thisSessionKey;
	private String serviceVersion = "$Revision: 1.16 $";
	
	public SadlServerMDImpl() {
		super();
	}
	
	public SadlServerMDImpl(String kbroot) throws ConfigurationException {
		super(kbroot);
	}
	
	public SadlServerMDImpl(String modelsFolder, String moduleName) throws ConfigurationException, ReasonerNotFoundException, SessionNotFoundException {
		super();
		thisSessionKey = super.selectServiceModel(modelsFolder, moduleName);
	}
	
	private String[][] getDataAsStringArray(String query) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, SessionNotFoundException, QueryCancelledException {
		ResultSet results = query(query);
		if (results != null) {
			int colCnt = results.getColumnCount();
			int rowCnt = results.getRowCount();
			String[][] modified = new String[rowCnt + 1][colCnt]; 
			for (int i = 0; i < colCnt; i++) {
				modified[0][i] = results.getColumnNames()[i];
			}
			for (int i = 0; i < rowCnt; i++) {
				for (int j = 0; j < colCnt; j++) {
					modified[i + 1][j] = results.getResultAt(i, j).toString();
				}
			}
			return modified;
		}
		return null;
	}
	
	/**
	 * This method returns the query associated with this service model name
	 * @return -- query associated with the given modelName
	 */
	public static String getModelQuery(String modelName) {
//		String query = "construct { ?s ?p ?o} where { {<http://djstservice#ESN123456> <iws:module> ?m . ?m <iws:component> ?s . ?s ?p ?o . FILTER (sameTerm(?p, <iws:remaining_cycles>))}  UNION {<http://djstservice#ESN123456> <iws:module> ?m . ?m <iws:component> ?s . ?s ?p ?o . FILTER (sameTerm(?p, <llp_life_limit>))} UNION  { <http://djstservice#ESN123456> <iws:module> ?m . ?m <iws:component> ?s . OPTIONAL {?s ?p ?o . FILTER (sameTerm(?o, <iws:Replace>) ) }  . OPTIONAL {?s ?p ?o . FILTER (sameTerm(?o, <iws:Continue>) ) } . FILTER (sameTerm(?p, <iws:disposition>) ) } UNION { <http://djstservice#ESN123456> <iws:module> ?s . OPTIONAL { ?s ?p ?o . FILTER (sameTerm(?o, <iws:Heavy> ) ) } . OPTIONAL { ?s ?p ?o . FILTER (sameTerm(?o, <iws:Light> ) ) } . OPTIONAL { ?s ?p ?o . FILTER (sameTerm(?o, <iws:TC> ) ) } } UNION { OPTIONAL {?s ?p ?o . FILTER (sameTerm(?p, <http://www.w3.org/2000/01/rdf-schema#comment>) && regex(str(?s), \"djstservice\") )} } }";
		String resourceBundleName = "queries";
		ResourceBundle rb = ResourceBundle.getBundle (resourceBundleName) ;
		String query = (String) rb.getString(modelName);
		return query;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#setOutputFormat(java.lang.String)
	 */
	public void setOutputFormat(String outputFormat) {
		if (reasoner != null) {
			reasoner.setOutputFormat(outputFormat);
		}
		this.outputFormat = outputFormat;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getOutputFormat()
	 */
	public String getOutputFormat() {
		return outputFormat;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getSessionKey()
	 */
	public String getSessionKey() {
		return thisSessionKey;
	}
	
	@Override
	public String[] getAllSubclassesOfTaxonomy(String root) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException, QueryParseException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?et where {?et <http://www.w3.org/2000/01/rdf-schema#subClassOf> <" + root + "> }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
	 		return retVal;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getDirectSubclassesOfTaxonomy(java.lang.String)
	 */
	@Override
	public String[] getDirectSubclassesOfTaxonomy(String root) 
			throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException,
					QueryParseException, SessionNotFoundException, QueryCancelledException {
			
		String query = "select distinct ?et where {?et <urn:x-hp-direct-predicate:http_//www.w3.org/2000/01/rdf-schema#subClassOf> <"+ root + "> }";
		
		return doQueryAndReturnStrings(query);
	}

	/**
	 * Return all of the ancestor class names given a starting class in a hierarchy
	 * 
	 * @param className, the starting class type name 
	 * @return an array of ancestor class type names or null if className is already a root
	 * @throws InvalidClassDomainException, InvalidConfigurationException
	 */
	public String[] getAncestorClassesOfTaxonomy(String className)
			 throws InvalidNameException, ReasonerNotFoundException, ConfigurationException,
			 		NameNotFoundException, QueryParseException, SessionNotFoundException, 
			 		QueryCancelledException {

		String query = "select distinct ?ancestors where ";
		query += "{<"+ className + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?mid .";
		query += " ?mid <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?ancestors . }";
		query += " group by ?ancestors order by count(?mid)";
		
		return doQueryAndReturnStrings(query);
	}

	/**
	 * Returns an array of class type names of all of the direct (one level up)
	 * ancestors in a class hierarchy.
	 * 
	 * @param className, the starting class type name 
	 * @return class names of all of the direct ancestors, or null if none were found
	 * @throws InvalidClassDomainException, InvalidConfigurationException
	 */
	public String[] getDirectSuperclassesOfTaxonomy(String className)
			 throws InvalidNameException, ReasonerNotFoundException, ConfigurationException,
		 		NameNotFoundException, QueryParseException, SessionNotFoundException, 
		 		QueryCancelledException {
		
		String query = "select distinct ?et where {<"+ className + "> <urn:x-hp-direct-predicate:http_//www.w3.org/2000/01/rdf-schema#subClassOf> ?et }";

		return doQueryAndReturnStrings(query);
	}


	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getLeafClassesOfTaxonomy(java.lang.String)
	 */
	public String[] getLeafClassesOfTaxonomy(String root) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?et where {?et <http://www.w3.org/2000/01/rdf-schema#subClassOf> <" + root + "> . OPTIONAL {?et2 <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?et . FILTER ((?et2 != <http://www.w3.org/2002/07/owl#Nothing> && ?et2 != ?et)) } FILTER (!bound(?et2)) }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
	 		return retVal;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getInstancesOfClass(java.lang.String)
	 */
	public String[] getInstancesOfClass(String cls) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?i where { ?i <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <" + cls + "> }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
	 		return retVal;
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#isObjectProperty(java.lang.String)
	 */
	public boolean isObjectProperty(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?t where {<" + property + "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?t . FILTER(?t = <http://www.w3.org/2002/07/owl#ObjectProperty>)}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			return true;
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#isDatatypeProperty(java.lang.String)
	 */
	public boolean isDatatypeProperty(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?t where {<" + property + "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?t . FILTER(?t = <http://www.w3.org/2002/07/owl#DatatypeProperty>)}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			return true;
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getPropertyDomain(java.lang.String)
	 */
	public String[] getPropertyDomain(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?d where { <" + property + "> <http://www.w3.org/2000/01/rdf-schema#domain> ?d }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
		return null;		
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getPropertyRange(java.lang.String)
	 */
	public String[] getPropertyRange(String property) throws NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?r where { <" + property + "> <http://www.w3.org/2000/01/rdf-schema#range> ?r }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
		return null;		
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getRequiredRangeClassesOfPropertyOfClass(java.lang.String, java.lang.String)
	 */
	public String[] getRequiredRangeClassesOfPropertyOfClass(String cls, String property) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		int numResults = 0;
		// qualified cardinality restriction
		String query = "select ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ";
		query += "?r <http://www.w3.org/2002/07/owl#onProperty> <http://www.mobius.illinois.edu/advise/ont/core/System#function>  . ";
		query += "?r <owl:minQualifiedCardinality> ?mc . ?r <http://www.w3.org/2002/07/owl#onClass> ?v . filter(?mc > 0)}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		if (results != null) {
			numResults =+ results.length - 1;
		}
		query = "select ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ";
		query += "?r <http://www.w3.org/2002/07/owl#onProperty> <http://www.mobius.illinois.edu/advise/ont/core/System#function>  . ";
		query += "?r <owl:qualifiedCardinality> ?qc . ?r <http://www.w3.org/2002/07/owl#onClass> ?v . filter(?qc > 0)}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results2 = getDataAsStringArray(query);
		if (results2 != null) {
			numResults += results2.length - 1;
		}

		query = "select ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ";
		query += "?r <http://www.w3.org/2002/07/owl#someValuesFrom> ?v}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results3 = getDataAsStringArray(query);
		if (results3 != null) {
			numResults += results3.length - 1;
		}
		
		String[] retVal = null;
		int retValIdx = 0;
		if (numResults > 0) {
			retVal = new String[numResults];
			if (results != null) {
				int size = results.length;
				for (int i = 0; i < size; i++) {
					String[] ri = results[i];
					for (int j = 0; j < ri.length; j++) {
						String rj = ri[j];
		//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
						if (i > 0) {
							retVal[retValIdx++] = rj;
						}
					}
				}
			}
			if (results2 != null) {
				int size = results2.length;
				for (int i = 0; i < size; i++) {
					String[] ri = results2[i];
					for (int j = 0; j < ri.length; j++) {
						String rj = ri[j];
		//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
						if (i > 0) {
							retVal[retValIdx++] = rj;
						}
					}
				}
			}
			if (results3 != null) {
				int size = results3.length;
				for (int i = 0; i < size; i++) {
					String[] ri = results3[i];
					for (int j = 0; j < ri.length; j++) {
						String rj = ri[j];
		//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
						if (i > 0) {
							retVal[retValIdx++] = rj;
						}
					}
				}
			}
		}
		return retVal;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getAllowedRangeClassesOfPropertyOfClass(java.lang.String, java.lang.String)
	 */
	public String[] getAllowedRangeClassesOfPropertyOfClass(String cls, String property) throws NameNotFoundException, InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Restriction> . ?r <http://www.w3.org/2002/07/owl#allValuesFrom> ?v . ?r <http://www.w3.org/2002/07/owl#onProperty> <" + property + ">}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		// these results will be 
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
		else {
			// just return the range?
			query = "select ?rng where {<" + property + "> <http://www.w3.org/2000/01/rdf-schema#range> ?rng}";
			if (SadlUtils.queryContainsQName(query)) {
				query = prepareQuery(query);
			}
			results = getDataAsStringArray(query);
			// these results will be 
			if (results != null) {
				int size = results.length;
				String[] retVal = new String[size - 1];
				for (int i = 0; i < size; i++) {
					String[] ri = results[i];
					for (int j = 0; j < ri.length; j++) {
						String rj = ri[j];
		//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
						if (i > 0) {
							retVal[i-1] = rj;
						}
					}
				}
				return retVal;
			}
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getAllowedValuesOfObjectPropertyOfClass(java.lang.String, java.lang.String)
	 */
	public String[] getAllowedValuesOfObjectPropertyOfClass(String cls, String property) throws IOException, NameNotFoundException, QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
//		String query = "select ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Restriction> . ?r <http://www.w3.org/2002/07/owl#onProperty> <" + property + "> . ?r <http://www.w3.org/2002/07/owl#allValuesFrom> ?o . ?o <http://www.w3.org/2002/07/owl#oneOf> ?l . ?l <http://jena.hpl.hp.com/ARQ/list#member> ?v}";
		String query = "select ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ?r <http://www.w3.org/2002/07/owl#onProperty> <" + property + "> . ?r <http://www.w3.org/2002/07/owl#allValuesFrom> ?v}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		if (results != null) { 
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; i++) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; j++) {
					String rj = ri[j];
	//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
	 		return retVal;
		}
		else {
			query = "select ?v where { <" + property + "> <http://www.w3.org/2000/01/rdf-schema#range> ?c . ?v <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?c}";
			if (SadlUtils.queryContainsQName(query)) {
				query = prepareQuery(query);
			}
			results = getDataAsStringArray(query);
			if (results != null) { 
				int size = results.length;
				String[] retVal = new String[size - 1];
				for (int i = 0; i < size; i++) {
					String[] ri = results[i];
					for (int j = 0; j < ri.length; j++) {
						String rj = ri[j];
		//				if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
						if (i > 0) {
							retVal[i-1] = rj;
						}
					}
				}
		 		return retVal;
			}
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getAllowedValuesOfDataPropertyOfClass(java.lang.String, java.lang.String)
	 */
	public Object[] getAllowedValuesOfDataPropertyOfClass(String cls, String property) throws QueryParseException, ReasonerNotFoundException, InvalidNameException, ConfigurationException, SessionNotFoundException, QueryCancelledException {
		String query = "select distinct ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Restriction> . ?r <http://www.w3.org/2002/07/owl#onProperty> <" + property + "> . ?r <http://www.w3.org/2002/07/owl#hasValue> ?v}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		ResultSet results = query(query);
		if (results != null) {
			int size = results.getRowCount();
			Object[] retVal = new Object[size];
			int i = 0;
			while (results.hasNext()) {		
				retVal[i++] = results.next()[0];
			}
	 		return retVal;
		}
		else {		
			query = "select distinct ?v where { <" + cls + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?r . ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Restriction> . ?r <http://www.w3.org/2002/07/owl#onProperty> <" + property + "> . ?r <http://www.w3.org/2002/07/owl#allValuesFrom> ?dr . ?dr <http://www.w3.org/2002/07/owl#oneOf> ?l . ?l <http://jena.hpl.hp.com/ARQ/list#member> ?v}";
			if (SadlUtils.queryContainsQName(query)) {
				query = prepareQuery(query);
			}
			results = query(query);
			if (results != null) {
				if (results != null) {
					int size = results.getRowCount();
					Object[] retVal = new Object[size];
					int i = 0;
					while (results.hasNext()) {		
						retVal[i++] = results.next()[0];
					}
			 		return retVal;
				}
			}
		}
		return null;
	}

	@Override
	public String[] getPropertiesWithGivenClassInDomain(String cls) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException {
		String query = "select distinct ?p where { ?p <http://www.w3.org/2000/01/rdf-schema#domain> <" + cls + ">}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		ResultSet results = query(query);
		if (results != null) {
			int size = results.getRowCount();
			String[] retVal = new String[size];
			int i = 0;
			while (results.hasNext()) {		
				retVal[i++] = (String) results.next()[0];
			}
	 		return retVal;
		}
		return null;
	}

	@Override
	public Object getDefaultValueOfPropertyOnClass(String cls, String prop) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException, QueryParseException, SessionNotFoundException, QueryCancelledException {
		String query = "select ?dv where {<" + cls + "> <http://www.w3.org/2000/01/rdf-schema#seeAlso> ?sa . ?sa <http://research.ge.com/Acuity/defaults.owl#appliesToProperty> <" + prop + "> . ?sa <http://research.ge.com/Acuity/defaults.owl#hasDataDefault> ?dv }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		ResultSet results = query(query);
		if (results != null) {
			int colCnt = results.getColumnCount();
			int rowCnt = results.getRowCount();
			if (colCnt > 1 || rowCnt > 1) {
				throw new ConfigurationException("Default of '" + prop + "' on class '" + cls + "' appears to have more than one default value.");
			}
			return results.getResultAt(0, 0);
		}
		// maybe it wasn't a Datatype property...
		query = "select ?dv where {<" + cls + "> <http://www.w3.org/2000/01/rdf-schema#seeAlso> ?sa . ?sa <http://research.ge.com/Acuity/defaults.owl#appliesToProperty> <" + prop + "> . ?sa <http://research.ge.com/Acuity/defaults.owl#hasObjectDefault> ?dv }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		results = query(query);
		if (results != null) {
			int colCnt = results.getColumnCount();
			int rowCnt = results.getRowCount();
			if (colCnt > 1 || rowCnt > 1) {
				throw new ConfigurationException("Default of '" + prop + "' on class '" + cls + "' appears to have more than one default value.");
			}
			return results.getResultAt(0, 0);
		}
		return null;
	}

	@Override
	public String[] getConceptRdfsLabels(String conceptUri) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException {
		String query = "select distinct ?l where { <" + conceptUri + "> <http://www.w3.org/2000/01/rdf-schema#label> ?l}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		ResultSet results = query(query);
		if (results != null) {
			int size = results.getRowCount();
			String[] retVal = new String[size];
			int i = 0;
			while (results.hasNext()) {		
				retVal[i++] = (String) results.next()[0];
			}
	 		return retVal;
		}
		return null;
	}

	@Override
	public String[] getConceptRdfsComments(String conceptUri) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException {
		String query = "select distinct ?c where { <" + conceptUri + "> <http://www.w3.org/2000/01/rdf-schema#comment> ?c}";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		ResultSet results = query(query);
		if (results != null) {
			int size = results.getRowCount();
			String[] retVal = new String[size];
			int i = 0;
			while (results.hasNext()) {		
				retVal[i++] = (String) results.next()[0];
			}
	 		return retVal;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.service.extended.ISadlExtendedService2#getAnnotation(java.lang.String)
	 */
	@Override
	public String getAnnotation(String className, String annotationName) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException {
		String query = "select ?d where { <" + className + "> <"+annotationName+"> ?d }";
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		ResultSet results = query(query);

		if ((results == null) || (results.getRowCount() == 0) || (results.getRowCount() > 1)) {
			return null;
		}
		
		return (String) results.first()[0];
	}

	private String[] doQueryAndReturnStrings(String query) 
			throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException,
					QueryParseException, SessionNotFoundException, QueryCancelledException {
			
		if (SadlUtils.queryContainsQName(query)) {
			query = prepareQuery(query);
		}
		String[][] results = getDataAsStringArray(query);
		if (results != null) {
			int size = results.length;
			String[] retVal = new String[size - 1];
			for (int i = 0; i < size; ++i) {
				String[] ri = results[i];
				for (int j = 0; j < ri.length; ++j) {
					String rj = ri[j];
//					if (logger.isDebugEnabled()) logger.debug("i="+ i + ", j=" + j + ", value=" + rj);
					if (i > 0) {
						retVal[i-1] = rj;
					}
				}
			}
			return retVal;
		}
 		return null;
	}

//	/**
//	 * Method to test an OWL output format to see if it is supported
//	 */
//	static public boolean isOwlFormatValid(String format) {
//		if (format != null && 
//				(format.equalsIgnoreCase("N-TRIPLE") ||
//				format.equalsIgnoreCase("TURTLE") ||
//				format.equalsIgnoreCase("N3") ||
//				format.equalsIgnoreCase("RDF/XML") ||
//				format.equalsIgnoreCase("RDF/XML-ABBREV"))) {
//			return true;
//		}
//		return false;
//	}
//
//	public Map<String, String[]> getServiceNameMap() {
//		return getServiceNameMap();
//	}
//
//	public String getClassName() throws SessionNotFoundException {
//		return getClassName();
//	}
//
//	public String getServiceVersion() throws SessionNotFoundException {
//		return getServiceVersion();
//	}
//
//	public ResultSet[] atomicQuery(String serviceName, DataSource dataSrc,
//			String inputFormat, String[] sparql) throws IOException,
//			ConfigurationException, NamedServiceNotFoundException,
//			QueryCancelledException, QueryParseException,
//			ReasonerNotFoundException, SessionNotFoundException,
//			InvalidNameException {
//		return atomicQuery(serviceName, dataSrc, inputFormat, sparql);
//	}
//
//	public ResultSet[] atomicQueryCsvData(String serviceName,
//			DataSource csvDataSrc, boolean includesHeader, String csvTemplate,
//			String[] sparql) throws IOException, ConfigurationException,
//			NamedServiceNotFoundException, QueryCancelledException,
//			QueryParseException, ReasonerNotFoundException,
//			SessionNotFoundException, InvalidNameException, TemplateException {
//		return atomicQueryCsvData(serviceName, csvDataSrc, includesHeader, csvTemplate, sparql);
//	}
//
//	public ResultSet ask(String subjName, String propName, Object objValue)
//			throws TripleNotFoundException, ReasonerNotFoundException,
//			QueryCancelledException, SessionNotFoundException {
//		return ask(subjName, propName, objValue);
//	}
//
//	public String prepareQuery(String query) throws InvalidNameException,
//			ReasonerNotFoundException, ConfigurationException,
//			InvalidNameException, SessionNotFoundException {
//		return prepareQuery(query);
//	}
//
//	public boolean loadData(String serverDataLocator) throws IOException,
//			ReasonerNotFoundException, SessionNotFoundException,
//			ConfigurationException {
//		return srvr.loadData(serverDataLocator);
//	}
//
//	public boolean loadCsvData(String serverCsvDataLocator,
//			boolean includesHeader, String serverCsvTemplateLocator)
//			throws TemplateException, ConfigurationException, IOException,
//			InvalidNameException, SessionNotFoundException, TemplateException {
//		return srvr.loadCsvData(serverCsvDataLocator, includesHeader, serverCsvTemplateLocator);
//	}
//
//	public String selectServiceModel(String serviceName)
//			throws ConfigurationException, ReasonerNotFoundException,
//			NamedServiceNotFoundException, SessionNotFoundException {
//		return srvr.selectServiceModel(serviceName);
//	}
//
//	public String selectServiceModel(String serviceName,
//			List<ConfigurationItem> preferences) throws ConfigurationException,
//			ReasonerNotFoundException, NamedServiceNotFoundException {
//		return srvr.selectServiceModel(serviceName, preferences);
//	}
//
//	public String selectServiceModel(String knowledgeBaseIdentifier,
//			String modelName) throws ConfigurationException,
//			ReasonerNotFoundException, SessionNotFoundException {
//		return srvr.selectServiceModel(knowledgeBaseIdentifier, modelName);
//	}
//
//	public String selectServiceModel(String knowledgeBaseIdentifier,
//			String modelName, List<ConfigurationItem> preferences)
//			throws ConfigurationException, ReasonerNotFoundException {
//		return srvr.selectServiceModel(knowledgeBaseIdentifier, modelName, preferences);
//	}
//
//	public boolean sendData(DataSource dataSrc) throws IOException,
//			ReasonerNotFoundException, SessionNotFoundException,
//			ConfigurationException {
//		return srvr.sendData(dataSrc);
//	}
//
//	public boolean sendData(DataSource dataSrc, String inputFormat)
//			throws IOException, ReasonerNotFoundException,
//			SessionNotFoundException, ConfigurationException {
//		return srvr.sendData(dataSrc, inputFormat);
//	}
//
//	public String setInstanceDataNamespace(String namespace)
//			throws InvalidNameException, SessionNotFoundException {
//		return srvr.setInstanceDataNamespace(namespace);
//	}
//
//	public boolean sendCsvData(DataSource csvDataSrc, boolean includesHeader,
//			String csvTemplate) throws TemplateException,
//			ConfigurationException, IOException, InvalidNameException,
//			SessionNotFoundException {
//		return srvr.sendCsvData(csvDataSrc, includesHeader, csvTemplate);
//	}
//
//	public boolean addTriple(String subjName, String predName, Object objValue)
//			throws ConfigurationException, TripleNotFoundException,
//			ReasonerNotFoundException, InvalidNameException,
//			SessionNotFoundException, ConfigurationException {
//		return srvr.addTriple(subjName, predName, objValue);
//	}
//
//	public String createInstance(String name, String className)
//			throws ConfigurationException, InvalidNameException, IOException,
//			SessionNotFoundException {
//		return srvr.createInstance(name, className);
//	}
//
//	public boolean deleteTriple(String subjName, String predName,
//			Object objValue) throws ConfigurationException,
//			TripleNotFoundException, ReasonerNotFoundException,
//			InvalidNameException, SessionNotFoundException,
//			ConfigurationException {
//		return srvr.deleteTriple(subjName, predName, objValue);
//	}
//
//	public boolean reset() throws ReasonerNotFoundException,
//			SessionNotFoundException {
//		return srvr.reset();
//	}
//
//	public void setOwlFileOutputFormat(String outputFormat)
//			throws SessionNotFoundException {
//		srvr.setOwlFileOutputFormat(outputFormat);
//	}
//
//	public String getKBaseIdentifier() throws ConfigurationException {
//		return srvr.getKBaseIdentifier();
//	}
//
//	public String getModelName() throws IOException {
//		return srvr.getModelName();
//	}
//
//	public String getReasonerVersion() throws ConfigurationException,
//			SessionNotFoundException {
//		return srvr.getReasonerVersion();
//	}
//
//	public DataSource getDerivations() throws ConfigurationException,
//			InvalidDerivationException, SessionNotFoundException {
//		return srvr.getDerivations();
//	}
//
//	public DataSource construct(String sparql) throws QueryCancelledException,
//			QueryParseException, SessionNotFoundException {
//		return srvr.construct(sparql);
//	}
//
//	public void collectTimingInformation(boolean bCollect)
//			throws SessionNotFoundException {
//		srvr.collectTimingInformation(bCollect);
//	}
//
//	public ReasonerTiming[] getTimingInformation()
//			throws SessionNotFoundException {
//		return srvr.getTimingInformation();
//	}
//
//	public void setKbaseRoot(String kbaseRoot) throws ConfigurationException {
//		srvr.setKbaseRoot(kbaseRoot);
//	}
//
//	public String getKbaseRoot() {
//		return srvr.getKbaseRoot();
//	}
//
//	public void setQueryTimeout(long timeout) throws ReasonerNotFoundException,
//			SessionNotFoundException {
//		srvr.setQueryTimeout(timeout);
//	}
//
//	public boolean clearCache() throws InvalidNameException,
//			SessionNotFoundException {
//		return srvr.clearCache();
//	}
//
//	public void setServiceNameMap(Map<String, String[]> serviceNameMap) {
//		srvr.setServiceNameMap(serviceNameMap);
//	}
//
//	public boolean persistInstanceModel(String owlInstanceFileName,
//			String globalPrefix) throws ConfigurationException,
//			SessionNotFoundException {
//		return srvr.persistInstanceModel(owlInstanceFileName, globalPrefix);
//	}
//
//	public boolean persistInstanceModel(String modelName,
//			String owlInstanceFileName, String globalPrefix)
//			throws ConfigurationException, SessionNotFoundException {
//		return srvr.persistInstanceModel(modelName, owlInstanceFileName, globalPrefix);
//	}
//
//	public boolean persistChangesToServiceModels()
//			throws SessionNotFoundException {
//		return srvr.persistChangesToServiceModels();
//	}
//
//	public List<ModelError> getErrors() throws SessionNotFoundException {
//		return srvr.getErrors();
//	}
//
//	public boolean addTriple(String modelName, String subject,
//			String predicate, Object value) throws ConfigurationException,
//			TripleNotFoundException, ReasonerNotFoundException,
//			SessionNotFoundException {
//		return srvr.addTriple(modelName, subject, predicate, value);
//	}
//
//	public String createInstance(String modelName, String name, String className)
//			throws ConfigurationException, InvalidNameException,
//			SessionNotFoundException, IOException {
//		return srvr.createInstance(modelName, name, className);
//	}
//
//	public boolean deleteTriple(String modelName, String subject,
//			String predicate, Object value) throws ConfigurationException,
//			TripleNotFoundException, ReasonerNotFoundException,
//			SessionNotFoundException {
//		return srvr.deleteTriple(modelName, subject, predicate, value);
//	}
//
//	public boolean addClass(String modelName, String className,
//			String superClassName) throws SessionNotFoundException,
//			InvalidNameException {
//		return srvr.addClass(modelName, className, superClassName);
//	}
//
//	public String getUniqueInstanceUri(String namespace, String baseLocalName)
//			throws InvalidNameException, SessionNotFoundException {
//		return srvr.getUniqueInstanceUri(namespace, baseLocalName);
//	}
//
//	public boolean addInstance(String modelName, String instName,
//			String className) throws ConfigurationException,
//			InvalidNameException, SessionNotFoundException {
//		return srvr.addInstance(modelName, instName, className);
//	}
//
//	public boolean addOntProperty(String modelName, String propertyName,
//			String superPropertyName) throws SessionNotFoundException,
//			InvalidNameException {
//		return srvr.addOntProperty(modelName, propertyName, superPropertyName);
//	}
//
//	public boolean addMaxCardinalityRestriction(String modelName,
//			String className, String propertyName, int cardValue)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addMaxCardinalityRestriction(modelName, className, propertyName, cardValue);
//	}
//
//	public boolean addMinCardinalityRestriction(String modelName,
//			String className, String propertyName, int cardValue)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addMinCardinalityRestriction(modelName, className, propertyName, cardValue);
//	}
//
//	public boolean addCardinalityRestriction(String modelName,
//			String className, String propertyName, int cardValue)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addCardinalityRestriction(modelName, className, propertyName, cardValue);
//	}
//
//	public boolean addHasValueRestriction(String modelName, String className,
//			String propertyName, String valueInstanceName)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addHasValueRestriction(modelName, className, propertyName, valueInstanceName);
//	}
//
//	public boolean addSomeValuesFromRestriction(String modelName,
//			String className, String propertyName, String restrictionName)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addSomeValuesFromRestriction(modelName, className, propertyName, restrictionName);
//	}
//
//	public boolean addAllValuesFromRestriction(String modelName,
//			String className, String propertyName, String restrictionName)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addAllValuesFromRestriction(modelName, className, propertyName, restrictionName);
//	}
//
//	public boolean createServiceModel(String kbid, String serviceName,
//			String modelName, String owlFileName)
//			throws SessionNotFoundException {
//		return srvr.createServiceModel(kbid, serviceName, modelName, owlFileName);
//	}
//
//	public boolean addOntPropertyDomainClass(String modelName,
//			String propertyName, String domainClassName)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addOntPropertyDomainClass(modelName, propertyName, domainClassName);
//	}
//
//	public boolean addObjectPropertyRangeClass(String modelName,
//			String propertyName, String rangeClassName)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.addObjectPropertyRangeClass(modelName, propertyName, rangeClassName);
//	}
//
//	public boolean setDatatypePropertyRange(String modelName,
//			String propertyName, String xsdRange)
//			throws SessionNotFoundException, InvalidNameException {
//		return srvr.setDatatypePropertyRange(modelName, propertyName, xsdRange);
//	}
//
//	public boolean addRule(String modelName, String ruleAsString)
//			throws ConfigurationException, IOException,
//			SessionNotFoundException {
//		return srvr.addRule(modelName, ruleAsString);
//	}
//
//	public boolean deleteModel(String modelName) throws ConfigurationException,
//			IOException, SessionNotFoundException {
//		return srvr.deleteModel(modelName);
//	}

}
