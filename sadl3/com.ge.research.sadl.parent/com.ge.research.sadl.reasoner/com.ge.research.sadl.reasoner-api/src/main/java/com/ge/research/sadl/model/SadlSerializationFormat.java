package com.ge.research.sadl.model;

import org.apache.jena.datatypes.xsd.impl.RDFjson;
import org.apache.jena.riot.RDFFormat;

import com.ge.research.sadl.reasoner.TranslationException;
/**
 * Class to contain the various formats in which SADL models may be persisted
 * @author 200005201
 *
 */
public class SadlSerializationFormat {
	public static final String TURTLE_FORMAT = "Turtle";
	public static final String N3_FORMAT = "N3";
	public static final String N_TRIPLE_FORMAT = "N-Triples";
	public static final String RDF_XML_FORMAT = "RDF/XML"; // default
	public static final String RDF_XML_ABBREV_FORMAT = "RDF/XML-ABBREV";
	public static final String JSON_LD_FORMAT = "JSON-LD";
	public static final String RDF_JSON_FORMAT = "RDF/JSON";
	public static final String TRIG_FORMAT = "TriG";
	public static final String N_QUADS_FORMAT = "N-Quads";
	public static final String TRIX_FORMAT = "TriX";
	public static final String RDF_BINARY_FORMAT = "RDF Binary";
	public static final String JENA_TDB_FORMAT = "Jena TDB";
	public static final String JENA_TDB2_FORMAT = "Jena TDB2";
	
	static RDFFormat TDB_PseudoFormat = RDFFormat.RDFNULL;
		
	/**
	 * Method to get the jena RDFFormat type from the SADL format type string
	 * @param sadlFormat
	 * @return
	 * @throws TranslationException
	 */
	public static RDFFormat getRDFFormat(String sadlFormat) throws TranslationException {
		if (sadlFormat != null) {
			if (sadlFormat.equals(TURTLE_FORMAT)) {
				return RDFFormat.TURTLE;
			} else if (sadlFormat.equals(N3_FORMAT)) {
				return RDFFormat.TURTLE;
			} else if (sadlFormat.equals(N_TRIPLE_FORMAT)) {
				return RDFFormat.NTRIPLES;
			} else if (sadlFormat.equals(RDF_XML_FORMAT)) {
				return RDFFormat.RDFXML;
			} else if (sadlFormat.equals(RDF_XML_ABBREV_FORMAT)) {
				return RDFFormat.RDFXML_ABBREV;
			} else if (sadlFormat.equals(RDF_JSON_FORMAT)) {
				return RDFFormat.RDFJSON;
			} else if (sadlFormat.equals(JSON_LD_FORMAT)) {
				return RDFFormat.JSONLD;
			} else if (sadlFormat.equals(TRIG_FORMAT)) {
				return RDFFormat.TRIG;
			} else if (sadlFormat.equals(N_QUADS_FORMAT)) {
				return RDFFormat.NQUADS;
			} else if (sadlFormat.equals(TRIX_FORMAT)) {
				return RDFFormat.TRIX;
			} else if (sadlFormat.equals(RDF_BINARY_FORMAT)) {
				return RDFFormat.RDF_THRIFT;
			} else if (sadlFormat.equals(JENA_TDB_FORMAT)) {
				return TDB_PseudoFormat;
			} else if (sadlFormat.equals(JENA_TDB2_FORMAT)) {
				return TDB_PseudoFormat;
			}
		}
		throw new TranslationException("Invalid format: " + sadlFormat);
	}
	
	/**
	 * Method to validate a SADL format string
	 * @param sadlFormat
	 * @return -- true if valid else false
	 */
	public static boolean validateSadlFormat(String sadlFormat) {
		if (sadlFormat == null) {
			return false;
		}
		if (sadlFormat.equals(JENA_TDB_FORMAT)) {
			return true;
		}
		try {
			if (getRDFFormat(sadlFormat) != null) {
				return true;
			}
		} catch (TranslationException e) {
			return false;
		}
		return false;
	}
	
	/**
	 * Method to get the file extension used for files from the Jena RDFFormat type
	 * @param rdfFormat
	 * @return
	 * @throws TranslationException
	 */
	public static String getFileExtension(RDFFormat rdfFormat) throws TranslationException {
		if (rdfFormat != null) {
			if (rdfFormat.equals(RDFFormat.TURTLE)) {
				return "ttl";
			}
			else if (rdfFormat.equals(RDFFormat.NTRIPLES)) {
				return "nt";
			}
			else if (rdfFormat.equals(RDFFormat.RDFXML)) {
				return "owl";
			}
			else if (rdfFormat.equals(RDFFormat.RDFXML_ABBREV)) {
				return "owl";
			}
			else if (rdfFormat.equals(RDFFormat.RDFJSON)) {
				return "rj";
			}
			else if (rdfFormat.equals(RDFFormat.JSONLD)) {
				return "jsonld";
			}
			else if (rdfFormat.equals(RDFFormat.TRIG)) {
				return "trig";
			}
			else if (rdfFormat.equals(RDFFormat.NQUADS)) {
				return "nq";
			}
			else if (rdfFormat.equals(RDFFormat.TRIX)) {
				return "trix";
			}
			else if (rdfFormat.equals(RDFFormat.RDF_THRIFT)) {
				return "trdf";
			}
			else if (rdfFormat.equals(TDB_PseudoFormat)) {
				return "tdb";
			}
		}
		throw new TranslationException("Invalid format: " + rdfFormat);
	}
	
	public static String getSadlSerializationFormatFromFilename(String owlFilename) {
		if (owlFilename.endsWith(".owl")) return RDF_XML_FORMAT;
		if (owlFilename.endsWith(".nt")) return N_TRIPLE_FORMAT;
		if (owlFilename.endsWith(".turtle")) return TURTLE_FORMAT;
		if (owlFilename.endsWith(".ttl")) return TURTLE_FORMAT;
		if (owlFilename.endsWith(".n3")) return TURTLE_FORMAT;
		if (owlFilename.endsWith(".rj")) return RDF_JSON_FORMAT;
		if (owlFilename.endsWith(".jsonld")) return JSON_LD_FORMAT;
		if (owlFilename.endsWith(".trig")) return TRIG_FORMAT;
		if (owlFilename.endsWith(".nq")) return N_QUADS_FORMAT;
		if (owlFilename.endsWith(".trix")) return TRIX_FORMAT;
		if (owlFilename.endsWith(".trdf")) return RDF_BINARY_FORMAT;
		if (owlFilename.endsWith(".tdb")) return JENA_TDB_FORMAT;
		return "RDF/XML";	// default

	}
	/** 
	 * Method to get the SADL format string from the Jena RDFFormat type
	 * @param rdfFormat
	 * @return
	 * @throws TranslationException
	 */
	public static String getSadlSerializationFormat(RDFFormat rdfFormat) throws TranslationException {
		if (rdfFormat != null) {
			if (rdfFormat.equals(RDFFormat.TURTLE)) {
				return TURTLE_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.NTRIPLES)) {
				return N_TRIPLE_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.RDFXML)) {
				return RDF_XML_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.RDFXML_ABBREV)) {
				return RDF_XML_ABBREV_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.RDFJSON)) {
				return RDF_JSON_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.JSONLD)) {
				return JSON_LD_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.TRIG)) {
				return TRIG_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.NQUADS)) {
				return N_QUADS_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.TRIX)) {
				return TRIX_FORMAT;
			}
			else if (rdfFormat.equals(RDFFormat.RDF_THRIFT)) {
				return RDF_BINARY_FORMAT;
			}
			else if (rdfFormat.equals(TDB_PseudoFormat)) {
				return JENA_TDB_FORMAT;
			}
		}
		throw new TranslationException("Unsupported format: " + rdfFormat);
	}
}

