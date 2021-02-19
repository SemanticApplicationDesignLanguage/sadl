package com.ge.research.sadl.jena;

import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.ext.xerces.impl.dv.XSSimpleType;

public class SadlXSDDatatype extends XSDDatatype {

    /**
     * Hidden constructor used when loading in external user defined XSD
     * types
     * 
     * @param xstype
     *            the XSSimpleType definition to be wrapped
     * @param namespace
     *            the namespace for the type (used because the grammar
     *            loading doesn't seem to keep that)
     */
	SadlXSDDatatype(XSSimpleType xstype, String namespace) {
        super(xstype, namespace);
    }
}
