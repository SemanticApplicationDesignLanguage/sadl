/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.4 $ Last modified on   $Date: 2015/07/25 16:26:47 $
 ***********************************************************************/

package com.ge.research.sadl.model;

import java.util.Objects;

import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;

import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.InvalidNameException;

/**
 * This class encapsulates a named concept in a SADL model. The SADL model
 * text may identify the concept only by name, or by prefix and name if it
 * is a named concept in another model. By calling the ModelManager an
 * instance of this class may be further attributed to include the namespace
 * of the concept (including the namespace of the current model if the 
 * concept is defined in the current model) and the concept type.
 * Note that a named SADL concept can be a class, property, instance,
 * or variable. A concept is often not known explicitly to be a variable but
 * is eventually inferred to be a variable because it is not defined to be 
 * a class, property, or instance.
 * 
 * @author crapo
 *
 */
public class ConceptName extends ConceptIdentifier {

	/**
	 * This enum defines the concept types in a non-Jena way that can be used
	 * throughout the SADL code
	 */
	public enum ConceptType {
		ANNOTATIONPROPERTY, RDFPROPERTY, DATATYPEPROPERTY, OBJECTPROPERTY, ONTCLASS, ONTCLASSLIST, INDIVIDUAL, MODELNAME, 
		RDFDATATYPE, RDFDATATYPELIST, VARIABLE, CONCEPT_NOT_FOUND_IN_MODEL, FUNCTION_DEFN
	}

	public enum RangeValueType {CLASS_OR_DT, LIST, UNION_OF_CLASSES, INTERSECTION_OF_CLASSES}

    private String name;
    private String prefix = null;
    private String namespace = null;;
    private ConceptType type = null;
    private RangeValueType rangeValueType = RangeValueType.CLASS_OR_DT;	// default
    
    /**
     * This constructor takes a single String input. However, this String may
     * be a compound name of the form <prefix>:<localname>, in which case the
     * input name is split to set the prefix and the name.
     * 
     * @param _name -- the input name, localname or compound <prefix>:<localname>
     */
    public ConceptName(String _name) {
    	if (_name == null) {
    		throw new NullPointerException("ConceptName constructor called with null name--please fix call.");
    	}
    	int hash = _name.indexOf('#');
    	if (hash > 0 && hash < _name.length() - 1) {
    		setNamespace(_name.substring(0, hash + 1));
    		setName(_name.substring(hash + 1));
    	}
    	else {
		    int colon = _name.indexOf(':');
		    if (!_name.startsWith("http:") && !_name.startsWith("file:") && colon > 0 && colon < _name.length() - 1) {
		        setPrefix(_name.substring(0, colon));
		        setName(_name.substring(colon + 1));
		    }
		    else {
		        setPrefix(null);
		        setName(_name);
		    }
    	}
    }
    
    /**
     * This constructor allows the ConceptType to be set in the call.
     * @param name
     * @param ctype
     */
    public ConceptName(String name, ConceptType ctype) {
    	this(name);
    	setType(ctype);
    }
    
    /**
     * This constructor is used when the prefix and the localname are both known.
     * 
     * @param _prefix
     * @param _name
     * @throws InvalidNameException
     */
    public ConceptName(String _prefix, String _name) throws InvalidNameException {
    	if (_name == null) {
    		throw new NullPointerException("ConceptName constructor called with null name--please fix call.");
    	}
        setPrefix(_prefix);
        setName(_name);
    }
    
    /**
     * This constructor adds the ConceptType to the call
     * @param _prefix
     * @param _name
     * @param ctype
     * @throws InvalidNameException
     */
    public ConceptName(String _prefix, String _name, ConceptType ctype) throws InvalidNameException {
    	this(_prefix, _name);
    	setType(ctype);
    }

    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }

    public String getPrefix() {
        return prefix;
    }

    private void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
    
    public String toString() {
    	if (prefix != null) {
            return prefix + ":" + name;
        }
    	else if (namespace != null) {
    		try {
				return getUri();
			} catch (InvalidNameException e) {
				// this is ok for local names
			}
    	}
        return name;
    }
    
    public String toFQString() {
    	try {
			return getUri();
		} catch (InvalidNameException e) {
			// this is ok for local names
		}
		return name;
    }
    
    public String getUri() throws InvalidNameException {
    	return getUri(null);
    }
    
    public String getUri(IConfigurationManager confMgr) throws InvalidNameException {
    	if (getNamespace() != null) {
    		return getNamespace() + getName();
    	}
    	else if (getPrefix() != null) {
    		if (getPrefix().equals("rdf")) {
    			setNamespace(RDF.getURI());
    			return RDF.getURI() + getName();
    		}
    		else if (getPrefix().equals("rdfs")) {
    			setNamespace(RDFS.getURI());
    			return RDFS.getURI() + getName();
    		}
    		else if (getPrefix().equals("owl")) {
    			setNamespace(OWL.getURI());
    			return OWL.getURI() + getName();
    		}
    		else if (getPrefix().equals("xsd")) {
    			setNamespace(XSD.getURI());
    			return XSD.getURI() + getName();
    		}
    		else if (confMgr != null) {
    			String uri = confMgr.getUriFromGlobalPrefix(getPrefix());
    			if (uri != null) {
    				if (!uri.endsWith("#")) {
    					uri += "#";
    				}
    				setNamespace(uri);
    				return uri + getName();
    			}
    		}
    	}
    	throw new InvalidNameException("ConceptName '" + toString() + "' doesn't have a namespace");
    }
    
    public boolean hasPrefix() {
        if (prefix != null) {
            return true;
        }
        return false;
    }

	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	public String getNamespace() {
		return namespace;
	}

	public void setType(ConceptType type) {
		this.type = type;
	}

	public ConceptType getType() {
		return type;
	}
	
	public boolean equals(ConceptName cn) {
		if (getName().equals(cn.getName())) {
			if (getNamespace() == null && cn.getNamespace() == null) {
				if (getPrefix() == null && cn.getPrefix() == null) {
					return true;
				}
			}
			if (getNamespace() != null && cn.getNamespace() != null &&
					getNamespace().equals(cn.getNamespace())) {
				return true;
			}
			if (getPrefix() != null && cn.getPrefix() != null && 
					getPrefix().equals(cn.getPrefix())) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * This version is needed to handle comparisons in lists, etc.
	 */
	@Override
	public boolean equals(Object o) {
		if (o instanceof ConceptName) {
			return equals((ConceptName)o);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return Objects.hash(name, namespace, prefix);
	}

	public RangeValueType getRangeValueType() {
		if (type.equals(ConceptType.ONTCLASSLIST) || type.equals(ConceptType.RDFDATATYPELIST))
			return RangeValueType.LIST;
		else
			return RangeValueType.CLASS_OR_DT;
	}
}
