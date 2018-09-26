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

package com.ge.research.sadl.server;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.naming.NameNotFoundException;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.server.SessionNotFoundException;

/**
 * This Interface class defines additional methods beyond those in 
 * the super classes SadlServer and SadlServerPE. These methods are 
 * primarily convenience functions to support model-driven application,
 * e.g., model-driven user-interface.
 * MD--model-driven
 * => SadlServerMD
 *  
 * @author Andy Crapo
 *
 */public interface ISadlServerMD extends ISadlServerPE {

	/**
	 * This method sets the output format of OWL files returned by certain queries.
	 * @param outputFormat
	 */
	public abstract void setOutputFormat(String outputFormat);

	/**
	 * This method retrieves the OWL output format setting.
	 * @return
	 */
	public abstract String getOutputFormat();

	public abstract String getSessionKey();
	
	/**
	 * This method returns all of the subclasses of the classification hierarchy starting at root
	 * @param root
	 * @return
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws SessionNotFoundException 
	 * @throws QueryParseException 
	 * @throws NameNotFoundException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getAllSubclassesOfTaxonomy(String root) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException, QueryParseException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns all of the direct subclasses (one level down) of root in a class hierarchy.
	 * 
	 * @param root, the starting class type name 
	 * @return class names of all of the direct descendants, or null if none were found
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws SessionNotFoundException 
	 * @throws QueryParseException 
	 * @throws NameNotFoundException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getDirectSubclassesOfTaxonomy(String root) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException, QueryParseException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns all of the ancestor classes given a starting class,
	 *  "className", in a class hierarchy.
	 * 
	 * @param root, the starting class type name 
	 * @return class names of all of the ancestors, or null if none were found
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws SessionNotFoundException 
	 * @throws QueryParseException 
	 * @throws NameNotFoundException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getAncestorClassesOfTaxonomy(String className) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException, QueryParseException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns all of the direct super classes (one level up) given
	 * a starting class, "className", in a class hierarchy.
	 * 
	 * @param root, the starting class type name 
	 * @return class names of all of the direct ancestors, or null if none were found
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws SessionNotFoundException 
	 * @throws QueryParseException 
	 * @throws NameNotFoundException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getDirectSuperclassesOfTaxonomy(String className) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, NameNotFoundException, QueryParseException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns all the leaf classes of the classification hierarchy starting at root
	 * @param root -- the localname, prefix:localname, or complete URI of the root class
	 * @return - a String array containing the local names of the leaf classes
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public abstract String[] getLeafClassesOfTaxonomy(String root)
			throws IOException, NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, URISyntaxException;

	/**
	 * This method returns all the instances of the given class
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array containing the localnames of the instances of the class
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public abstract String[] getInstancesOfClass(String cls)
			throws IOException, NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, URISyntaxException;

	/**
	 * This method returns true if and only if the property is an ObjectProperty
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - true if an ObjectProperty else false
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract boolean isObjectProperty(String property)
			throws NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns true if and only if the property is an DatatypeProperty
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - true if an DatatypeProperty else false
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract boolean isDatatypeProperty(String property)
			throws NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns the class(es) which are the domain of the property, if any.
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - a String array containing the domain classes
	 * @throws NameNotFoundException 
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getPropertyDomain(String property)
			throws NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns the class(es) or XSD types which are the range of the property, if any.
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @return - a String array containing the range classes or XSD types
	 * @throws NameNotFoundException 
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getPropertyRange(String property)
			throws NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns all the classes that must be represented in the range of the given property on the given class
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array containing the range classes that are required to be represented in the values of the property
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public abstract String[] getRequiredRangeClassesOfPropertyOfClass(
			String cls, String property) throws IOException,
			NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, URISyntaxException;

	/**
	 * This method returns all the names of classes that are allowed in the range of the given property on the given class
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array containing the range classes that are allowed to be represented in the values of the property
	 * @throws NameNotFoundException 
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryParseException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getAllowedRangeClassesOfPropertyOfClass(
			String cls, String property) throws NameNotFoundException,
			InvalidNameException, ReasonerNotFoundException,
			ConfigurationException, QueryParseException,
			SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns all the names of instances that are allowed values of the given property on the given class
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - a String array of the allowed value instances
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public abstract String[] getAllowedValuesOfObjectPropertyOfClass(
			String cls, String property) throws IOException,
			NameNotFoundException, QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, URISyntaxException;

	/**
	 * This method returns all the allowed values of the given property on the given class. Note that in the case of xsd:string values,
	 * the values will be wrapped in double quotes.
	 * @param property -- the localname, prefix:localname, or complete URI of the property
	 * @param cls -- the localname, prefix:localname, or complete URI of the class
	 * @return - an Object array of the allowed values, which will be of type Integer, String, Float, etc.
	 * @throws IOException
	 * @throws NameNotFoundException
	 * @throws ReasonerNotFoundException 
	 * @throws QueryParseException 
	 * @throws ConfigurationException 
	 * @throws InvalidNameException 
	 * @throws SessionNotFoundException 
	 * @throws QueryCancelledException 
	 * @throws URISyntaxException 
	 */
	public abstract Object[] getAllowedValuesOfDataPropertyOfClass(String cls,
			String property) throws QueryParseException,
			ReasonerNotFoundException, InvalidNameException,
			ConfigurationException, SessionNotFoundException,
			QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns all of the properties that have the given class in the property domain.
	 * @param cls
	 * @return
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws QueryParseException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getPropertiesWithGivenClassInDomain(String cls) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException, IOException, URISyntaxException;
	
	/**
	 * This method returns the default value of the given property on the given class
	 * @param cls
	 * @param prop
	 * @return -- default value if it exists else null if it doesn't
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws SessionNotFoundException 
	 * @throws QueryParseException 
	 * @throws NameNotFoundException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract Object getDefaultValueOfPropertyOnClass(String cls, String prop) throws InvalidNameException, ReasonerNotFoundException, 
		ConfigurationException, NameNotFoundException, QueryParseException, SessionNotFoundException, QueryCancelledException, IOException, URISyntaxException;
	
	/**
	 * This method returns the rdfs:label values (aka aliases, aka longnames) for a given concept identified by URI
	 * @param conceptUri -- the URI of the concept
	 * @return -- a String array of labels if any else null
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws QueryParseException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getConceptRdfsLabels(String conceptUri) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException, IOException, URISyntaxException;
	
	/**
	 * This method returns the rdfs:comment values (aka notes, aka descriptions) for a given concept identified by URI
	 * @param conceptUri -- the URI of the concept
	 * @return -- a String array of comments if any else null
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws QueryParseException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String[] getConceptRdfsComments(String conceptUri) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException, IOException, URISyntaxException;

	/**
	 * This method returns the annotation on the given class
	 * @param className
	 * @param annotationName
	 * @return -- the annotation String if any else null
	 * @throws ConfigurationException 
	 * @throws ReasonerNotFoundException 
	 * @throws InvalidNameException 
	 * @throws QueryCancelledException 
	 * @throws QueryParseException 
	 * @throws URISyntaxException 
	 * @throws IOException 
	 */
	public abstract String getAnnotation(String className, String annotationName) throws InvalidNameException, ReasonerNotFoundException, ConfigurationException, QueryParseException, QueryCancelledException, IOException, URISyntaxException ;

}