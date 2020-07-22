/************************************************************************
 * Copyright � 2007-2011 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.jena.importer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.activation.URLDataSource;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.OntResource;
import org.apache.jena.ontology.Ontology;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.RDFWriter;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.OWL2;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.importer.AbortDataRowException;
import com.ge.research.sadl.importer.ITabularDataImporter;
import com.ge.research.sadl.importer.SkipGroupException;
import com.ge.research.sadl.importer.SkipTripleException;
import com.ge.research.sadl.importer.TemplateException;
import com.ge.research.sadl.model.ConceptName;
import com.ge.research.sadl.model.ConceptName.ConceptType;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.reasoner.CircularDependencyException;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.ConfigurationManagerFactory;
import com.ge.research.sadl.reasoner.ConfigurationManagerForEditing;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.QueryParseException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.reasoner.utils.StringDataSource;

import au.com.bytecode.opencsv.CSVParser;
import au.com.bytecode.opencsv.CSVReader;

/**
 * This class converts a CSV file to an OWL model.
 * 
 * @author 200005201
 *
 */
public class CsvImporter implements ITabularDataImporter {
	private static final String CSV_IMPORTER = "CsvImporter";
	private static final Logger logger = LoggerFactory.getLogger(CsvImporter.class);
	public enum NewResourceType {Class, Property, Individual}
	public enum Type {NonBlank, ExistsInModel, TriplePattern, String, Boolean, Decimal, Integer, 
		DateTime, Time, Date, HexBinary, Base64Binary, AnyUri}
	public enum Disposition {SkipTriple, SkipGroup, Abort, Generate}

	private File triplesAddedInOrder = null; 
	private BufferedWriter triplesLoggerOut = null;

	private String modelFolderName = null;
	//	private String csvFilename = null;
	private InputStream csvInputStream = null;
	private boolean includesHeader = false;
	private String importModelNS = null;
	private String importModelPrefix = null;
	private String[] imports = null;
	private Map<String, Validate> validates = null;
	private Map<String, String> generatedValues = null;
	private Map<String, Transform> transforms = null;
	private Map<String, String> transformedValues = null;
	private List<Triple> templates = null;
	private Map<Integer, GroupOfTriples> groups = null;
	private GroupOfTriples activeGroup = null;
	private Map<String, Object> varMap = null;

	private String owlModelFormat = SadlSerializationFormat.RDF_XML_ABBREV_FORMAT;	// default format
	private OntModel models[];					// an array to hold the collection of models which may be used. 
	// this array should be sized to the value in "numThreads"
	private Dataset tdbDS = null;				// the TDB repository to which the import is adding triples
	private String tdbFolder = null;			// the TDB repository folder path

	private IConfigurationManager configMgr = null;

	private ExecutorService executor;				// an executor for our threadpool

	private String saveAsFileName = null;
	private boolean incremental = false;
	private boolean infer = false;
	private int chunkSize = -1;
	private int numThreads = 1;
	private IReasoner reasoners[];
	private boolean allowTripleProcessingLog = true;

	private long numTriplesImported = 0;
	private int rowNum;
	private int modelsInUse = 1;
	private OntModel importModel = null;	// the model of all imports, with import statements removed, for use with import of TDB inferred model
	private List<String> indirectImportNamespaces = null;	// the namespaces that are not direct imports, needed to resolve unqualified local names
	private SadlUtils sadlUtils = null;
	private boolean processed = false;	// has this set of inputs been processed?

	/**
	 * Inner class to represent an RDF Triple pattern with the objects it contains and
	 * from which the actual triples will be constructed.
	 * 
	 * @author 200005201
	 *
	 */
	public class Triple {
		Object subject;
		Object predicate;
		Object object;

		public Triple(Object subj, Object pred, Object obj) {
			subject = subj;
			predicate = pred;
			object = obj;
		}

		public String toString() {
			return subject.toString() + " " + predicate.toString() + " " + object.toString();
		}

		public String toShortString() {
			StringBuilder sb = new StringBuilder();
			if (subject instanceof ConceptName) {
				sb.append(((ConceptName)subject).getName());
			}
			else if (subject instanceof Variable) {
				sb.append(((Variable)subject).toShortString());
			}
			else {
				sb.append(subject.toString());
			}
			sb.append(" ");
			if (predicate instanceof ConceptName) {
				sb.append(((ConceptName)predicate).getName());
			}
			else if (predicate instanceof Variable) {
				sb.append(((Variable)predicate).toShortString());
			}
			else {
				sb.append(predicate.toString());
			}
			sb.append(" ");
			if (object instanceof ConceptName) {
				sb.append(((ConceptName)object).getName());
			}
			else if (object instanceof Variable) {
				sb.append(((Variable)object).toShortString());
			}
			else {
				sb.append(object.toString());
			}
			return sb.toString();
		}
	}

	public class GroupIndex {
		private String name = null;
		private int curVal = -1;
		private int maxVal = -1;

		public GroupIndex(String _name) {
			name = _name;
		}

		public String getName() {
			return name;
		}

		public int getCurVal() {
			return curVal;
		}

		public void setCurVal(int curVal) {
			this.curVal = curVal;
		}

		public int getMaxVal() {
			return maxVal;
		}

		public void setMaxVal(int maxVal) {
			this.maxVal = maxVal;
		}
	}

	public class GroupOfTriples {
		private int startingTriple;
		private int endingTriple;
		private Map<String,GroupIndex> indices;

		public GroupOfTriples(int start) {
			startingTriple = start;
		}

		public int getEndingTriple() {
			return endingTriple;
		}

		public void setEndingTriple(int endingTriple) {
			this.endingTriple = endingTriple;
		}

		public int getStartingTriple() {
			return startingTriple;
		}

		public void addIndex(String idx) {
			if (indices == null) {
				indices = new Hashtable<String, GroupIndex>();
			}
			if (!indices.containsKey(idx)) {
				indices.put(idx, new GroupIndex(idx));
			}
		}

		public Map<String, GroupIndex> getIndices() {
			return indices;
		}
	}

	/**
	 * Class to encapsulate all of the information from a validate statement.
	 * 
	 * @author 200005201
	 *
	 */
	public class Validate {
		private String identifier = null;
		private Type type = null;
		private Disposition disposition = null;
		private Triple triple = null;
		private String baseUri = null;

		public Validate(String _identifier, Type _type, Disposition _disposition) {
			identifier = _identifier;
			type = _type;
			disposition = _disposition;
		}

		public Validate(Triple tr, Disposition _disposition) {
			triple = tr;
			type = Type.TriplePattern;
			disposition = _disposition;
		}

		public boolean isValid(Object value) {
			if (type.equals(Type.NonBlank)) {
				if (value != null && (!(value instanceof String) || ((String)value).length() > 0)) {
					return true;
				}
				else if (disposition.equals(Disposition.Generate)) {

				}
			}
			return false;
		}

		public String getIdentifier() {
			return identifier;
		}

		public Type getType() {
			return type;
		}

		public Disposition getDispostion() {
			return disposition;
		}

		public Triple getTriple() {
			return triple;
		}
	}
	
	/*	Methods to create and manage the array of models used in the importer.
	 * 	
	 */

	protected void allocateOntModelsArray(int modelCount){
		// use the value in numThreads to set up the collection of OntModels which
		// will be used in the multithreaded csv importer.

		if (models != null) {
			OntModel[] newArray = new OntModel[modelCount];
			System.arraycopy(models, 0, newArray, 0, models.length);
			models = newArray;

			for(int q = 1; q < models.length; q += 1){
				try {
					prepareNewModel(q);
				} catch (ConfigurationException e) {
					e.printStackTrace();
				}
			}

		} else {
			models = new OntModel[modelCount];
			for (int i = 0; i < modelCount; i++) {
				models[i] = null;
			}
		}
		reasoners = new IReasoner[modelCount];
		executor = Executors.newFixedThreadPool(modelCount); // set up the threadpool size
	}


	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#makeValidate(java.lang.String, int)
	 */
	@Override
	public Object[] makeValidate(String rawTemplate, int modelArrayPosition) throws TemplateException {
		// convert tabs to spaces
		if (rawTemplate.indexOf('\t') >= 0) {
			rawTemplate = rawTemplate.replace('\t', ' ');
		}
		String[] tokens = rawTemplate.trim().split(" ");
		// first token should be "validate"
		if (tokens.length > 2) {
			Type type = null;
			Disposition disp = Disposition.SkipTriple;
			// the second token should be the identifier
			String identifier = tokens[1].trim();
			Triple triple = null;
			if (identifier.startsWith("<") && identifier.endsWith(">")) {
				identifier = identifier.substring(1, identifier.length() - 1);
			}

			if (tokens.length > 3 && tokens[2].trim().toLowerCase().equals("not") && tokens[3].trim().toLowerCase().equals("blank")) {
				type = Type.NonBlank;
				if (tokens.length > 4 && tokens[4].trim().toLowerCase().equals("abort")) {
					disp = Disposition.Abort;
				}
				else if (tokens.length > 4 && tokens[4].trim().toLowerCase().equals("skip")) {
					if (tokens.length > 5) {
						if (tokens[5].trim().toLowerCase().equals("group")) {
							disp = Disposition.SkipGroup;
						}
						else if (tokens[5].trim().toLowerCase().equals("triple")) {
							disp = Disposition.SkipTriple;
						}
						else {
							throw new TemplateException("Validate not blank skip has invalid skip type '" + tokens[5] + "'");
						}
					}
				}
				else if (tokens.length > 4 && tokens[4].trim().toLowerCase().equals("generate")) {
					if (tokens.length < 6) {
						throw new TemplateException("Validate action " + tokens[4] + " must be followed by a base URI.");
					}
					else {
						disp = Disposition.Generate;
						String errors = SadlUtils.validateRdfUri(tokens[5].trim());
						if (errors != null) {
							throw new TemplateException("Validate not blank generate base URI (" + tokens[5] + ") invalid: " + errors);
						}
					}
				}
			}
			else if (tokens[2].trim().toLowerCase().equals("exists")) {
				type = Type.ExistsInModel;
				if (tokens.length > 3 && tokens[3].trim().toLowerCase().endsWith("abort") || 
						tokens.length > 4 && tokens[4].trim().toLowerCase().equals("abort") ||
						tokens.length > 5 && tokens[5].trim().toLowerCase().equals("abort")) {
					disp = Disposition.Abort;
				}
			}
			else if (matchType(tokens[2]) != null) {
				type = matchType(tokens[2]);
				if (tokens.length >= 4 && tokens[3].trim().toLowerCase().equals("skip")) {
					if (tokens.length == 5 && tokens[4].trim().toLowerCase().equals("group")) {
						disp = Disposition.SkipGroup;
					}
					else {
						disp = Disposition.SkipTriple;						
					}
				}
			}
			else if (tokens.length >= 4) {
				// this could be a triple
				if (tokens.length > 4) {
					if (tokens[4].trim().toLowerCase().endsWith("abort")) {
						disp = Disposition.Abort;
					}
				}
				try {
					Object subj = processNode(tokens[1], modelArrayPosition);
					Object pred = processNode(tokens[2], modelArrayPosition);
					String objToken = tokens[3];
					Object obj;
					obj = processObjectNode(pred, objToken, modelArrayPosition);
					if (obj == null) {
						throw new TemplateException("Object of validate triple (" + tokens[1] + ", " + tokens[2] + ", " + tokens[3] + ") could not be resolved.");
					}
					type = Type.TriplePattern;
					triple = new Triple(subj, pred, obj);

				} catch (Exception e) {
					e.printStackTrace();
					throw new TemplateException("Failed to convert statement '" + rawTemplate + "' to a validate statement. (" + e.getMessage() + ")");
				}
			}
			else {
				throw new TemplateException("Invalid data field identifer (" + identifier + ") in validate statement '" + rawTemplate + "'");
			}
			Object[] results = new Object[2];
			if (type.equals(Type.TriplePattern)) {
				results[0] = rawTemplate;
				results[1] = new Validate(triple, disp);
			}
			else if (type != null) {
				results[0] = identifier;
				results[1] = new Validate(identifier, type, disp);
				if (disp.equals(Disposition.Generate)) {
					((Validate)results[1]).baseUri = tokens[5].trim();
				}
			}
			return results;
		}
		else {
			throw new TemplateException("Incomplete template validate statement '" + rawTemplate + "'");
		}
	}

	private Type matchType(String match) {
		for (Type typ : Type.values()) {
			if (typ.name().toLowerCase().equals(match.trim().toLowerCase())) {
				return typ;
			}
		}
		return null;
	}

	/**
	 * Class to encapsulate all of the information from a transform statement.
	 * 
	 * @author 200005201
	 *
	 */
	public class Transform {
		private String inputIdentifier = null;
		private String outputIdentifier = null;
		protected CsvImporter importer = null;
		private Transform dependsOn = null;

		public Transform(CsvImporter _importer, String ident, String resultIdent) {
			importer = _importer;
			inputIdentifier = ident;
			String baseName = _importer.getBaseName(inputIdentifier);
			if (_importer.transforms.containsKey(baseName)) {
				dependsOn = _importer.transforms.get(baseName);
			}
			outputIdentifier = resultIdent;
		}

		public String getInputIdentifier() {
			return inputIdentifier;
		}

		protected void setInputIdentifier(String inputIdentifier) {
			this.inputIdentifier = inputIdentifier;
		}

		public String getOutputIdentifier() {
			return outputIdentifier;
		}

		protected void setOutputIdentifier(String outputIdentifier) {
			this.outputIdentifier = outputIdentifier;
		}

		public Object transform(String value, int modelArrayPosition) throws ConfigurationException, ReasonerNotFoundException, QueryParseException, QueryCancelledException {
			return value;
		}

	}

	public class Split extends Transform {
		private String delimiters = null;
		public Split(CsvImporter _importer, String ident, String resultIdent, String delim) {
			super(_importer, ident, resultIdent);
			delimiters = delim;
		}

		@Override
		public String[] transform(String input, int modelArrayPosition) {
			if (delimiters != null) {
				String[] tokens = input.split(delimiters);
				return tokens;
			}
			// null delimiters
			String[] tokens = new String[input.length()];
			for (int i = 0; i < input.length(); i++) {
				tokens[i] = input.substring(i, i+1);
			}
			return tokens;
		}
	}

	public class Encode extends Transform {
		private boolean urlEncode = false;
		private Map<String, String> replacements = null;
		public Encode(CsvImporter _importer, String ident, String resultIdent, String replacementText, boolean _urlEncode) throws IOException {
			super(_importer, ident, resultIdent);
			urlEncode = _urlEncode;
			if (replacementText != null && replacementText.length() > 1) {
				InputStream is = new ByteArrayInputStream(replacementText.getBytes()); 
				//    public CSVReader(Reader reader, char separator, char quotechar, char escape, int line, boolean strictQuotes, boolean ignoreLeadingWhiteSpace) {
				CSVReader reader = null;
				try {
					reader = new CSVReader(new InputStreamReader(is), 
						CSVParser.DEFAULT_SEPARATOR,					// comma
						CSVParser.DEFAULT_QUOTE_CHARACTER, 				// double quote
						//    		    		CSVParser.DEFAULT_ESCAPE_CHARACTER, 0, 			// back slash
						'\'', 0,
						false, 											// strict quotes
						false);											// ignore leading whitespace
					String [] tokens = reader.readNext();
					int numReplacements = (tokens != null ? tokens.length : 0);
					if (numReplacements > 0) {
						boolean parserUnbalancedQuotes = false;
						for (int i = 0; i < numReplacements; i++) {
							String repl = tokens[i];
							String[] parts = repl.split(":");
							if (parts != null && parts.length == 2) {
								if (replacements == null) {
									replacements = new HashMap<String, String>();
								}
								if (i == 0 && replacementText.startsWith("\"") && !parts[0].startsWith("\"") && parts[0].endsWith("\"")) {
									parserUnbalancedQuotes = true;
								}
								if (parserUnbalancedQuotes) {
									parts[0] = "\"" + parts[0];
									parts[1] = "\"" + parts[1];
								}
								replacements.put(getSadlUtils().stripQuotes(parts[0].trim()).trim(), getSadlUtils().stripQuotes(parts[1].trim()).trim());
							}
							else if (repl.endsWith(":")) {
								if (replacements == null) {
									replacements = new HashMap<String, String>();
								}
								replacements.put(getSadlUtils().stripQuotes(parts[0]), "");
	
							}
							else {
								throw new IOException("Failed to parse '" + replacementText + "' into a set of colon-separated pairs of replace-replacewith tokens in row " + rowNum + ".");
							}
						}
					}
					else if (urlEncode) {
						// can't have an Encode with no urlEncoding (a "replace") and no replacement
						throw new IOException("Can't have a 'replace' with no replacement text");
					}
				}
				finally {
					if (reader != null) {
						reader.close();
					}
				}
			}
		}

		@Override
		public String transform(String input, int modelArrayPosition) {
			if (transformedValues != null && transformedValues.containsKey(getInputIdentifier())) {
				return transformedValues.get(getInputIdentifier());
			}
			else {
				if (replacements != null) {
					Iterator<String> repItr = replacements.keySet().iterator();
					while (repItr.hasNext()) {
						String replace = repItr.next();
						String repWith = replacements.get(replace);
						String transformed = null;
						if (replace.length() == 0) {
							// this is replace for an empty value
							if (input.length() == 0) {
								// this is an empty value
								transformed = repWith;
								if (logger.isDebugEnabled()) {
									if (transformed == null || !transformed.equals(input)) {
										logger.debug("Encode (" + getInputIdentifier() + ")(" + replace + ":" + repWith + ") transformed '" + input + "' -> '" + transformed + "' (before URL encoding)");
									}
								}
								input = transformed;
							}
						}
						else {
							transformed = input.replaceAll(replace, repWith).trim();
							if (logger.isDebugEnabled()) {
								if (transformed == null || !transformed.equals(input)) {
									logger.debug("Encode (" + getInputIdentifier() + ")(" + replace + ":" + repWith + ") transformed '" + input + "' -> '" + transformed + "' (before URL encoding)");
								}
							}
							input = transformed;
						}
					}
				}
				try {
					if (urlEncode) {
						input = URLEncoder.encode(input, "UTF-8");
					}
				} catch (UnsupportedEncodingException e) {
					logger.error("Transform of input value '" + input + "' with transform '" + replacements.toString() + "' failed URL encoding.");
				}
				if (transformedValues == null) {
					transformedValues = new HashMap<String, String>();
				}
				transformedValues.put(getInputIdentifier(), input);
			}
			return input;
		}
	}

	public class Find extends Transform {
		private String query = null;

		public Find(CsvImporter _importer, String ident, String resultIdent, String _query) {
			super(_importer, ident, resultIdent);
			query = _query;
		}

		@Override
		public String[] transform(String input, int modelArrayPosition) throws ConfigurationException, ReasonerNotFoundException, QueryParseException, QueryCancelledException {
			String[] matches = null;
			IReasoner reasoner;
			if (reasoners != null && reasoners.length > modelArrayPosition && reasoners[modelArrayPosition] != null) {
				reasoner = reasoners[modelArrayPosition];
			}
			else {
				reasoner = getConfigMgr().getReasoner();
			}
			
			int iStatus = reasoner.initializeReasoner(modelFolderName,
					(importer.imports != null && importer.imports.length > 0) ? importer.imports[0] : null, SadlSerializationFormat.RDF_XML_ABBREV_FORMAT);
			if (iStatus == 0) {
				logger.error("Reasoner initialization returned failure status 0.");
			}
			// replace input variable in query
			String iid = "<" + this.getInputIdentifier() + ">";
			String modified = query.replaceAll(iid, input);
			ResultSet rs = null;
			try {
				modified = reasoner.prepareQuery(modified);
				rs = reasoner.ask(modified);
			} catch (InvalidNameException e) {
				throw new QueryParseException("Invalid name in query: " + e.getMessage(), e);
			} catch (QueryCancelledException e) {
				throw new QueryCancelledException("Query timed out: " + e.getMessage(), e);
			}
			if (rs != null) {
				//				int colcnt = rs.getColumnCount() ;
				int rowcnt = rs.getRowCount();
				matches = new String[rowcnt];
				for (int i = 0; i < rowcnt; i++) {
					matches[i] = rs.getResultAt(i, 0).toString();
				}
			}
			if (logger.isDebugEnabled()) {
				StringBuilder sb = new StringBuilder();
				for (int i = 0; matches != null && i < matches.length; i++) {
					if (i > 0) {
						sb.append(", ");
					}
					sb.append(matches[i]);
				}
				logger.debug("Find returning: " + sb.toString());
			}
			return matches;
		}
	}

	protected Object[] makeTransform(String rawTemplate) throws TemplateException {
		String[] tokens = rawTemplate.trim().split(" ");
		// first token should be "transform"
		if (tokens.length > 4) {
			// the second token should be the identifier
			String identifier = tokens[1].trim();
			if (identifier.startsWith("<") && identifier.endsWith(">")) {
				identifier = identifier.substring(1, identifier.length() - 1);
			}
			else {
				throw new TemplateException("Invalid data field identifer (" + identifier + ") in transform statement '" + rawTemplate + "'");
			}
			if (!tokens[2].trim().toLowerCase().equals("to")) {
				throw new TemplateException("Invalid syntax in transform statement '" + rawTemplate + "': expected 3rd token to be 'to'");
			}
			String resultIdentifier = tokens[3].trim();
			if (resultIdentifier.startsWith("<") && resultIdentifier.endsWith(">")) {
				resultIdentifier = getBaseName(resultIdentifier.substring(1, resultIdentifier.length() - 1));
			}
			else {
				throw new TemplateException("Invalid resulting data field identifer (" + resultIdentifier + ") in transform statement '" + rawTemplate + "'");
			}
			Object[] results = new Object[2];
			results[0] = resultIdentifier;

			if (tokens[4].trim().toLowerCase().equals("split")) {
				String delim = "[ ]+";
				if (tokens.length > 5) {
					int delimStart = rawTemplate.toLowerCase().indexOf(" split ") + 7;
					delim = rawTemplate.substring(delimStart).trim();
//					delim = tokens[5].trim();
					if (delim.startsWith("\"")) {
						delim = delim.substring(1, delim.length() - 1);	// remove double quotes
					}
				}
				else {
					delim = null;
				}
				results[1] = new Split(this, identifier, resultIdentifier, delim);
			}
			else if (tokens[4].trim().toLowerCase().equals("encode")) {
				int encodeLoc = rawTemplate.toLowerCase().indexOf("encode");
				String replacementText = rawTemplate.substring(encodeLoc + 6).trim();
				try {
					results[1] = new Encode(this, identifier, resultIdentifier, replacementText, true);
				} catch (IOException e) {
					throw new TemplateException(e.getMessage(), e);
				}
			}
			else if (tokens[4].trim().toLowerCase().equals("replace")) {
				int replaceLoc = rawTemplate.toLowerCase().indexOf("replace");
				String replacementText = rawTemplate.substring(replaceLoc + 7).trim();
				try {
					results[1] = new Encode(this, identifier, resultIdentifier, replacementText, false);
				} catch (IOException e) {
					throw new TemplateException(e.getMessage(), e);
				}
			}
			else if (tokens[4].trim().toLowerCase().equals("find")) {
				if (tokens.length > 5) {	
					int findLoc = rawTemplate.toLowerCase().indexOf("find");
					String query = rawTemplate.substring(findLoc + 4).trim();
					if (query.startsWith("\"")) {
						query = query.substring(1, query.length() - 1);
					}
					results[1] = new Find(this, identifier, resultIdentifier, query);
				}
			}
			return results;
		}
		else {
			throw new TemplateException("Incomplete template validate statement '" + rawTemplate + "'");
		}
	}

	/**
	 * Method to check an identifier to see if it is an array and if so return the base name (index info removed)
	 * 
	 * @param identifier
	 * @return
	 */
	private String getBaseName(String identifier) {
		if (identifier.indexOf('[') > 0 && identifier.indexOf(']') > 0) {
			identifier = identifier.substring(0, identifier.indexOf('['));
		}
		return identifier;
	}

	/**
	 * Inner class representing a variable in the template, which will eventually be used to create the 
	 * elements of the triples generated.
	 * 
	 * @author 200005201
	 *
	 */
	public class Variable {
		String prefix;
		String name;
		int column;
		private Object postfix;
		Resource bnodeResource;

		public Variable(String pre, String nm, Object post) {
			prefix = pre;
			name = nm;
			setPostfix(post);
		}

		public void setColumn(int col) {
			column = col;
		}

		public String toString() {
			return prefix + "<" + name + ">" + "(replace with column " + column + ")" + getPostfix();
		}

		public String toShortString() {
			return prefix + "<" + name + ">" + getPostfix();
		}

		Object getPostfix() {
			return postfix;
		}

		void setPostfix(Object postfix) {
			this.postfix = postfix;
		}
	}

	/**
	 * Class has a null argument constructor.
	 */
	public CsvImporter() {
		
	}

	public CsvImporter(IConfigurationManager configurationMgr) {
		configMgr = configurationMgr;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setModelFolder(java.lang.String)
	 */
	@Override
	public void setModelFolder(String _modelDir) throws IOException {
		_modelDir = validateFile(_modelDir, true);
		modelFolderName = _modelDir;
		if (!modelFolderName.startsWith(HTTP_URI_SCHEME)) {
			File mf = new File(modelFolderName);
			File pf = mf.getParentFile();
			if (pf.exists() && pf.isDirectory()) {
				pf.getCanonicalPath();
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setCsvFilename(java.lang.String, boolean)
	 */
	@Override
	public void setImportFilename(String _csvFilename, boolean _includesHeader) throws IOException {
		_csvFilename = validateFile(_csvFilename, false);
		csvInputStream = new FileInputStream(_csvFilename);
		setIncludesHeader(_includesHeader);
		processed = false; // set or reset
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setCsvDataSource(javax.activation.DataSource, boolean)
	 */
	@Override
	public void setImportDataSource(DataSource csvDs, boolean _includesHeader) throws IOException {
		csvInputStream = csvDs.getInputStream();
		setIncludesHeader(_includesHeader);
		processed = false; // set or reset
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setImportModelNamespace(java.lang.String)
	 */
	@Override
	public void setImportModelNamespace(String ns) throws MalformedURLException {
		ns = stripNamespaceDelimiter(ns);
		importModelNS = getSadlUtils().validateHTTP_URI(ns);
		processed = false; // set or reset
	}


	private String stripNamespaceDelimiter(String ns) {
		if (ns.endsWith("#")) {
			ns = ns.substring(0, ns.length() - 1);
		}
		return ns;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#getSaveAsFileName()
	 */
	@Override
	public String getSaveAsFileName() throws ConfigurationException {
		if (owlModelFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
			if (saveAsFileName == null || saveAsFileName.startsWith(HTTP_URI_SCHEME)) {
				if (importModelNS == null) {
					throw new ConfigurationException("Either the 'saveAsFileName' or the 'importModelNS' must be set; neither are.");
				}
				int lastSlash = importModelNS.lastIndexOf('/');
				String importModelNSSpecificPart = lastSlash > 0 ? importModelNS.substring(lastSlash + 1) : importModelNS;
				if (importModelNSSpecificPart.indexOf(':') >= 0) {
					importModelNSSpecificPart = importModelNSSpecificPart.replace(':', '_');
				}
				if (modelFolderName != null) {
					saveAsFileName = modelFolderName + "/" + importModelNSSpecificPart + ".TDB";
				}
				else {
					saveAsFileName = "./" + importModelNSSpecificPart + ".TDB";
				}
				logger.debug("saveAsFileName has been set to '" + saveAsFileName + "'");
			}
		}
		return saveAsFileName;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setSaveAsFileName(java.lang.String)
	 */
	@Override
	public void setSaveAsFileName(String saveAsFileName) {
		if (!incremental) {
			this.saveAsFileName = saveAsFileName;
		}
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setImports(java.util.List)
	 */
	@Override
	public void setImports(List<String> _imports) throws ConfigurationException {
		StringBuffer sb = new StringBuffer();
		for (int i=0; i<_imports.size(); i++) {
			if (i>0) {
				sb.append(",");
			}
			sb.append(_imports.get(i));
		}
		setImports(sb.toString());
		processed = false; // set or reset
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setImports(java.lang.String)
	 */
	@Override
	public void setImports(String _imports) throws ConfigurationException {
		StringTokenizer st = new StringTokenizer(_imports, ",");
		int tokenCount = st.countTokens();
		if (tokenCount > 0) {
			imports = new String[tokenCount];

			for (int i = 0; i < tokenCount; i++) {
				String rawImport = st.nextToken();
				imports[i] = rawImport;
			}
		}
		processed = false; // set or reset
	}

	private void prepareNewModel(int arraypos) throws ConfigurationException {
		getModel(arraypos).setNsPrefixes(getImportModel().getNsPrefixMap());
		Ontology ontology = null;
		if (importModelPrefix != null) {
			getModel(arraypos).setNsPrefix(importModelPrefix, getModelNamespace());
		}
		else {
			getModel(arraypos).setNsPrefix("", getModelNamespace());
		}
		ontology = getModel(arraypos).createOntology(importModelNS);
		ontology.addComment("This ontology was created from a CSV data source.", "en");
		
		for (int i = 0; imports != null &&  i < imports.length; i++) {
			String rawImport = imports[i];
			String publicUri = null;
			if (rawImport.toLowerCase().startsWith(HTTP_URI_SCHEME)) {
				publicUri = rawImport;
			}
			Resource importedOntology = getModel(arraypos).createResource(publicUri);
			getModel(arraypos).getOntology(importModelNS).addImport(importedOntology);
			if (owlModelFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
				getModel(arraypos).add(getImportModel());
			}
			else {
				// TODO : this will need to change for SadlServer/SadlServerPlus (awc, 5/28/2012)
				if (incremental) {
					IConfigurationManager cmgr = getConfigMgr();
					if (cmgr != null) {
						String alturl = cmgr.getAltUrlFromPublicUri(importModelNS);
						if (alturl != null) {
							getModel(arraypos).getDocumentManager().setProcessImports(true);
							try {
								getModel(arraypos).read(alturl);
							}
							catch (Throwable t) {
								// this is ok if there is no previous import
							}
						}
					}
				}
				getModel(arraypos).loadImports();
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setTemplates(java.lang.String)
	 */
	@Override
	public void setTemplates(String template) throws ConfigurationException, TemplateException {
		if (template == null || template.length() <= 0) {
			throw new ConfigurationException("template is null or empty");
		}
//		System.out.println("setTemplates: " + template);
		processed = false; // set or reset

		String templateString = null;
		allocateOntModelsArray(numThreads);		// this initially sets the array size to 1.
		// it is included in the case that no number was given for the "parallel" tag
		int modelArrayPosition = 0;

		try {
			if (template.startsWith(HTTP_URI_SCHEME)) {
				DataSource dataSrc;
				dataSrc = new URLDataSource(new URL(template));
				templateString = getSadlUtils().convertDataSourceToString(dataSrc);
				Object[] templateParts = scanTemplateForNameAndImports(templateString);
				setImportModelNamespace((String) templateParts[0]);
				if (templateParts[1] instanceof List<?>) {
					setImports((List<String>)templateParts[1]);
				}
				else {
					setImports((String)templateParts[1]);
				}
				templateString = (String) templateParts[2];
			} else if (template.startsWith("file:/")) {
				try {
					File templateFile = new File(new URL(template).toURI());
//					System.out.println("File from URL: " + templateFile.getCanonicalPath());
					DataSource dataSrc = new FileDataSource(templateFile);
					templateString = getSadlUtils().convertDataSourceToString(dataSrc);
				} catch (URISyntaxException e) {
					throw new TemplateException("Failed to convert argument '" + template + "' from URL to an actual file.");
				}
				Object[] templateParts = scanTemplateForNameAndImports(templateString);
				setImportModelNamespace((String) templateParts[0]);
				if (templateParts[1] instanceof List<?>) {
					setImports((List<String>)templateParts[1]);
				}
				else {
					setImports((String)templateParts[1]);
				}
				templateString = (String) templateParts[2];
			} else {
				templateString = template;
			}
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (templateString == null || templateString.length() < 1) {
			throw new TemplateException("Template after extracting name and imports is null");
		}
		List<String> templateList = new ArrayList<String>();
		String str;
		BufferedReader reader = new BufferedReader(new StringReader(templateString));

		try {
			while ((str = reader.readLine()) != null) {
				str = str.trim();
				if (str.startsWith("uri")) {
					int aliasIdx = str.indexOf("alias");
					if (aliasIdx > 0) {
						importModelPrefix = str.substring(aliasIdx + 5).trim();
					}
					continue;
				}
				else if (str.startsWith("import")) {
//					if (imports == null) {
//						imports = 
//					}
					// TODO make it handle imports from template
					continue;
				}
				else if (str.length() > 0 && !str.startsWith("//")) {
					templateList.add(str);
				}
			}

		} catch(IOException e) {
			e.printStackTrace();
			throw new ConfigurationException("IOException reading templates: " + e.getMessage());
		}  

		int tokenCount = templateList.size();
		if (tokenCount > 0) {
			int tripleCount = 0; 
			int validateCount = 0;
			int transformCount = 0;
			for ( int i = 0; i < tokenCount; i++) {
				String rawTemplate = templateList.get(i).trim();
				if (isTransform(rawTemplate)) {
					transformCount++;
				}
				else if (isValidate(rawTemplate)) {
					validateCount++;
				}
				else if (isIncremental(rawTemplate)) {
					logger.debug("template has incremental option");
				}
				else if (isInfer(rawTemplate)) {
					logger.debug("template has infer option, chunk size " + chunkSize);
				}
				else {
					tripleCount++;
				}
			}

			try {
				templates = new ArrayList<Triple>();
				transforms = new HashMap<String, Transform>();
				validates = new HashMap<String, Validate>();
				int iTriple = 0;
				int iTransform = 0;
				int lastTemplateLine = -1;
				boolean newModelPrepared = false;
				for (int i = 0; i < tokenCount; i++) {
					String rawTemplate = templateList.get(i);
					if (rawTemplate.trim().startsWith("uri")) {
						continue;
					}
					else if (rawTemplate.trim().startsWith("log")) {
						allowTripleProcessingLog = true;
						enableTriplesAddedInOrderLogging(rawTemplate.trim().substring(4).trim());
					}
					else if (isValidate(rawTemplate)) {
						Object[] values = makeValidate(rawTemplate, modelArrayPosition);
						validates.put((String)values[0], (Validate)values[1]);
					}
					else if (isTransform(rawTemplate)) {
						Object[] values = makeTransform(rawTemplate);
						if (transforms.containsKey((String)values[0])) {
							throw new ConfigurationException("Found duplicate transform result name '" + values[0] + "' in transform '" + rawTemplate + "'");
						}
						transforms.put((String)values[0], (Transform)values[1]);
					}
					else if (isIncremental(rawTemplate)) {
						if (rawTemplate != null && rawTemplate.length() >= 11 && rawTemplate.substring(0, 11).toLowerCase().equals("incremental")) {
							incremental = true;
							if (importModelNS != null) {
								try {
									saveAsFileName = getSadlUtils().fileUrlToFileName(getConfigMgr().getAltUrlFromPublicUri(importModelNS));
								} catch (MalformedURLException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							}
						}
					}
					else if (isInfer(rawTemplate)) {
						if (rawTemplate != null && rawTemplate.length() >= 5 && rawTemplate.substring(0, 5).toLowerCase().equals("infer")) {
							infer = true;
							int chkidx = rawTemplate.indexOf("chunk");
							if (chkidx > 0) {
								int len = 0;
								while (rawTemplate.length() > (chkidx + 6 + len) && Character.isDigit(rawTemplate.charAt(chkidx + 6 + len))) {
									len++;
								}
								String chunk = rawTemplate.substring(chkidx + 6, chkidx + 6 + len);
								chunkSize = Integer.parseInt(chunk.trim());
							}
							int pllidx = rawTemplate.indexOf("parallel");
							if (pllidx > 0) {
								int len = 0;
								while (rawTemplate.length() > (pllidx + 9 + len) && Character.isDigit(rawTemplate.charAt(pllidx + 9 + len))) {
									len++;
								}
								String threads = rawTemplate.substring(pllidx + 9, pllidx + 9 + len);
								numThreads  = Integer.parseInt(threads.trim());
								allocateOntModelsArray(numThreads);			// this is the actual setting of the count of the number of models to use.
								// it is only called in the event that the "parallel" keyword is used and 
								// followed by a number
							}
						}
					}
					else {
						int groupStartLine = -1;
						int groupEndLine = -1;
						boolean groupingOnly = false;
						if (rawTemplate.trim().equals("{")) {
							groupStartLine = templates.size();
							groupingOnly = true;
						}
						else if (rawTemplate.trim().equals("}")) {
							groupEndLine = lastTemplateLine;
							groupingOnly = true;
						}
						else if (rawTemplate.trim().startsWith("{")) {
							groupStartLine = templates.size();
							rawTemplate = rawTemplate.trim().substring(1);
						}
						else if (rawTemplate.trim().endsWith("}")) {
							groupEndLine = templates.size();
							int trimmedLen = rawTemplate.trim().length();
							rawTemplate = rawTemplate.trim().substring(0, trimmedLen - 1);
						}
						if (groupStartLine >= 0) {
							// this is the beginning of a group. The end of the group will always be the matched against the largest 
							//	starting group line that has a null end.
							getGroups().put(groupStartLine, null);
						}
						if (groupEndLine >= 0) {
							// this is the end of a group. Find the largest group start line with no end--this must be the end to that group
							if (groups != null) {
								int maxNull = -1;
								Iterator<Integer> gslIter = groups.keySet().iterator();
								while (gslIter.hasNext()) {
									Integer start = gslIter.next();
									GroupOfTriples end = groups.get(start);
									if (end == null && maxNull < start) {
										maxNull = start;
									}
								}
								GroupOfTriples gtr = new GroupOfTriples(maxNull);
								gtr.setEndingTriple(groupEndLine);
								groups.put(maxNull, gtr);
							}
						}
						if (importModelNS == null) {
							if (tripleCount == 1) {
								throw new ConfigurationException("No import model namespace specified; if input was a file name please prefix with 'file:/'.");
							}
							throw new ConfigurationException("No import model namespace specified.");
						}
						if (groupingOnly) {
							tripleCount--;
						}
						else {
							if (!newModelPrepared) {
								// we're to the triple patterns so now it's safe to prepare the model (infer, if present, is already processed)
								// (but only do it once--the first time)
								prepareNewModel(modelArrayPosition);
								newModelPrepared = true;
							}
							StringTokenizer st2 = new StringTokenizer(rawTemplate, " ");
							int tripleTokenCnt = st2.countTokens();
							if (tripleTokenCnt < 3) {
								throw new ConfigurationException("Template triple pattern '" + rawTemplate + "' does not have expected space-delimited parts: subject predicate object.");
							}
							String tkn1;
							String tkn2;
							String tkn3;
							if (tripleTokenCnt == 4) {
								tkn1 = st2.nextToken();
								tkn2 = st2.nextToken();
								tkn3 = st2.nextToken();
								if (tkn2.equalsIgnoreCase("is") && tkn3.equalsIgnoreCase("a")) {
									// predicate is "is a" => rdf:type
									tkn2 = "rdf:type";
									tkn3 = st2.nextToken();
									tripleTokenCnt = 3;
								}
								else if (tkn2.equalsIgnoreCase("has")) {
									// assume predicate is "has ..."; drop has
									tkn2 = tkn3;
									tkn3 = st2.nextToken();
									tripleTokenCnt = 3;
								}
							}
							else {
								tkn1 = st2.nextToken();
								tkn2 = st2.nextToken();
								tkn3 = st2.nextToken();
							}
							Object subj = processNode(tkn1, modelArrayPosition);
							Object pred = processNode(tkn2, modelArrayPosition);
							String objToken = tkn3;
							Object obj;
							try {
								obj = processObjectNode(pred, objToken, modelArrayPosition);
							} catch (Exception e) {
								e.printStackTrace();
								throw new ConfigurationException("Failed to convert object value '" + objToken + "' to a valid Literal.");
							}
							if (pred instanceof String && pred.equals("is") &&
									!(subj instanceof Variable) && obj instanceof ConceptName) {
								try {
									for (int modelct = 0; modelct < models.length; modelct++){
										// repeat for each model to create.
										if (((ConceptName)obj).getUri().equals(OWL.Class.getURI())) {
											getModel(modelct).createClass(getModelNamespace() + subj);
										}
										else if (((ConceptName)obj).getUri().equals(OWL.DatatypeProperty.getURI())) {
											getModel(modelct).createDatatypeProperty(getModelNamespace() + subj);
										}
										else if (((ConceptName)obj).getUri().equals(OWL.ObjectProperty.getURI())) {
											getModel(modelct).createObjectProperty(getModelNamespace() + subj);
										}

										/*
										 * Note: at this point, we have actually created models for each of the elements in the
										 * array. the getModel() method originally created something that was more or less a singleton
										 * and was re-purposed to create a collection.
										 */
									}

								} catch (InvalidNameException e) {
									throw new TemplateException("Unable to create template triple: " + e.getMessage(), e);
								}
							}
							else if (!(subj instanceof Variable) && !(pred instanceof Variable) && !(obj instanceof Variable)) {
								// If there aren't any variables then we should be able to do this just once, in the beginning
								try {
									Property prop = useTemplateProperty(pred, null, 0, modelArrayPosition);
									Resource subject;
									if (pred.equals(RDFS.subClassOf)) {
										subject = useTemplateResource(subj, null, prop, NewResourceType.Class, 0, modelArrayPosition);	
									}
									else {
										subject = useTemplateResource(subj, null, prop, NewResourceType.Individual, 0, modelArrayPosition);
									}
									RDFNode object = useTemplateRDFNode(obj, null, prop, 0, modelArrayPosition);
									if (subject != null && pred != null && obj != null) {
										addTripleToModel(subject, prop, object, -1, modelArrayPosition);
									}
								} catch (InvalidNameException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								} catch (AbortDataRowException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								} catch (ReasonerNotFoundException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								} catch (QueryParseException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								} catch (SkipTripleException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								} catch (SkipGroupException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								} catch (QueryCancelledException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							}
							else {
								Triple tr = new Triple(subj, pred, obj);
								templates.add(tr);
								iTriple++;
								lastTemplateLine = templates.size() - 1;
								if (hasWildCardIteration(tr)) {
									GroupOfTriples gtr = new GroupOfTriples(lastTemplateLine);
									gtr.setEndingTriple(lastTemplateLine);
									getGroups().put(lastTemplateLine, gtr);
								}
							}
						}
					}
				}
				if (groups == null) {
					// put all the triples in one group in case there are some indices
					GroupOfTriples gtr = new GroupOfTriples(0);
					gtr.setEndingTriple(templates.size() - 1);
					getGroups().put(0, gtr);
				}
				populateGroupIndices();
			}
			catch (Throwable t) {
				t.printStackTrace();
			}
			finally {
//				closeTdbDS();
			}
		}
	}

	private String getTripleElementAsString(RDFNode rsrc, int modelArrayPosition) throws ConfigurationException {
		if (rsrc instanceof Resource && ((Resource)rsrc).isURIResource()) {
			String prefix = getModel(modelArrayPosition).getNsURIPrefix(((Resource)rsrc).getNameSpace());
			if (prefix != null) {
				StringBuilder sb = new StringBuilder();
				if (prefix.length() > 0) {
					sb.append(prefix);
					sb.append(":");
				}
				if (rsrc.isURIResource()) {
					sb.append(((Resource)rsrc).getLocalName());
				}
				else {
					sb.append(rsrc.toString());
				}
				return sb.toString();
			}
		}
		return rsrc.toString();
	}

	private boolean hasWildCardIteration(Triple tr) {
		if (tr.subject instanceof Variable) {
			String idxnm = getIndex(((Variable)tr.subject).name);
			if (idxnm != null && idxnm.equals("*")) {
				return true;
			}
		}
		if (tr.predicate instanceof Variable) {
			String idxnm = getIndex(((Variable)tr.predicate).name);
			if (idxnm != null && idxnm.equals("*")) {
				return true;
			}
		}
		if (tr.object instanceof Variable) {
			String idxnm = getIndex(((Variable)tr.object).name);
			if (idxnm != null && idxnm.equals("*")) {
				return true;
			}
		}
		return false;
	}

	private Map<Integer, GroupOfTriples> getGroups() {
		if (groups == null) {
			groups = new HashMap<Integer, GroupOfTriples>();
		}
		return groups;
	}

	/**
	 * This method looks for array variables in the group with unbound named variables as the index. The existence of such named
	 * but unbound indices means that iteration over all values of each index must occur for the triples in the group. Note that
	 * an explicit numerical index does not require special handling in this way.
	 */
	private void populateGroupIndices() {
		if (groups != null) {
			Iterator<Integer> gitr = groups.keySet().iterator();
			while (gitr.hasNext()) {
				GroupOfTriples grp = groups.get(gitr.next());
				for (int itr = grp.getStartingTriple(); itr <= grp.getEndingTriple(); itr++) {
					Triple tr = templates.get(itr);
					if (tr.subject instanceof Variable) {
						if (isIndex((Variable)tr.subject)) {
							grp.addIndex(getIndex((Variable)tr.subject));
						}
					}
					if (tr.predicate instanceof Variable) {
						if (isIndex((Variable)tr.predicate)) {
							grp.addIndex(getIndex((Variable)tr.predicate));
						}
					}
					if (tr.object instanceof Variable) {
						if (isIndex((Variable)tr.object)) {
							grp.addIndex(getIndex((Variable)tr.object));
						}
					}
				}
			}
		}
	}
	
	private boolean isIndex(Variable var) {
		String name = var.name;
		if (isIndex(name)) {
			return true;
		}
		if (var.getPostfix() instanceof Variable) {
			return isIndex((Variable)var.getPostfix());
		}
		return false;
	}

	/**
	 * Method to determine if the specified name is a variable with an unbound named index.
	 * @param name
	 * @return
	 */
	private boolean isIndex(String name) {
		if (name == null) {
			return false;
		}
		String index = getIndex(name);
		if (index == null) {
			return false;
		}
		if (isStringAnInteger(index.trim())) {
			return false;
		}
		if (varMap.containsKey(index)) {
			return false;
		}
		if (transforms != null) {
			Iterator<String> keyIter = transforms.keySet().iterator();
			while (keyIter.hasNext()) {
				Transform trfm = transforms.get(keyIter.next());
				if (trfm.getOutputIdentifier().equals(name)) {
					return false;
				}
			}
		}
		return true;
	}

	private String getIndex(Variable var) {
		String name = var.name;
		String idx = getIndex(name);
		if (idx != null) {
			return idx;
		}
		if (var.getPostfix() instanceof Variable) {
			return getIndex((Variable)var.getPostfix());
		}
		return null;
	}

	private String getIndex(String name) {
		if (name != null) {
			int opSB = name.indexOf('[');
			if (opSB > 0) {
				int clSB = name.indexOf(']');
				if (opSB >0 && clSB > opSB) {
					return name.substring(opSB + 1, clSB);
				}
			}
		}
		return null;
	}

	private boolean isStringAnInteger(String str) {
		if (str != null) {
			for (int i = 0; i < str.length(); i++) {
				if (!Character.isDigit(str.charAt(i))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}
	
	private boolean isTransform(String line) {
		if (line != null && line.length() > 10 && line.substring(0, 10).toLowerCase().equals("transform ")) {
			return true;
		}
		return false;
	}

	private boolean isValidate(String line) {
		if (line != null && line.length() > 9 && line.substring(0, 9).toLowerCase().equals("validate ")) {
			return true;
		}
		return false;
	}

	private boolean isIncremental(String line) {
		if (line != null) {
			line = line.trim();
			if (line.length() >= 11 && line.substring(0, 11).toLowerCase().equals("incremental")) {
				incremental = true;
				return true;
			}
		}
		return false;
	}

	private boolean isInfer(String line) throws TemplateException {
		if (line != null) {
			line = line.trim();
			if (line.length() >= 5 && line.substring(0, 5).toLowerCase().equals("infer")) {
				try {
					infer = true;
					// inferred model only go to TDB for now
					setOwlModelFormat(SadlSerializationFormat.JENA_TDB_FORMAT);
					String[] tokens = line.split("\\s+");
					for (int i = 1; i < tokens.length; i+=2) {
						String token = tokens[i];
						if (i  + 1 < tokens.length) {
							String numStr = tokens[i+1];
							if (token.toLowerCase().equals("chunk")) {
								chunkSize = Integer.parseInt(numStr);
							}
							else if (token.toLowerCase().equals("parallel")) {
								numThreads = Integer.parseInt(numStr);
							}
						}
					}
				}
				catch (NumberFormatException e) {
					throw new TemplateException("Invalid 'infer' line: " + line);
				} catch (InvalidNameException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				return true;
			}
		}
		return false;
	}

	private Object processObjectNode(Object pred, String token, int modelArrayPosition) throws Exception {
		if (pred.equals("is")) {
			// special case
			return processNode(token, modelArrayPosition);
		}
		else if (pred instanceof ConceptName && ((ConceptName)pred).getUri().equals(RDFS.range.getURI())) {
			if (token.equals("string")) {
				return XSD.xstring;
			}
			else if (token.equals("boolean")) { 
				return XSD.xboolean;
			}
			else if (token.equals("decimal")) { 
				return XSD.decimal;
			}
			else if (token.equals("integer")) { 
				return XSD.integer;
			}
			else if (token.equals("dateTime")) { 
				return XSD.dateTime;
			}
			else if (token.equals("time")) { 
				return XSD.time;
			}
			else if (token.equals("date")) { 
				return XSD.date;
			}
			else if (token.equals("hexBinary")) { 
				return XSD.hexBinary;
			}
			else if (token.equals("base64Binary")) { 
				return XSD.base64Binary;
			}
			else if (token.equals("anyURI")) { 
				return XSD.anyURI;
			}
		}
		if (pred instanceof ConceptName) {
			boolean tokenContainsVariable = tokenContainsVariable(token);
			//			if (token.contains("<") && token.contains(">")) {
			//				tokenContainsVariable = true;
			//			}
			if (!tokenContainsVariable && ((ConceptName)pred).getType().equals(ConceptType.DATATYPEPROPERTY)) {
				// the predicate was found in the model so check its range
				OntProperty prop = getModel(modelArrayPosition).getOntProperty(((ConceptName)pred).getUri());
				Literal val = SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), prop, token);
				return val;
			}
			else {
				// it's an ObjectProperty
				return processNode(token, modelArrayPosition);
			}
		}
		else if (pred instanceof Variable) {
			return processNode(token, modelArrayPosition);
		}
		else {
			throw new TranslationException("Triple predicate of unhandled type: " + pred.toString());
		}
	}

	private Object processNode(String token, int modelArrayPosition) throws ConfigurationException {
		if (tokenContainsVariable(token)) {
			return tokenToVariable(token);
		}
		else {
			return tokenToConceptName(token, modelArrayPosition);
		}
	}

	private boolean tokenContainsVariable(String token) {
		if (token != null && (token.contains("<") || token.contains("row()"))) {
			return true;
		}
		return false;
	}

	protected Object tokenToVariable(String token)
			throws ConfigurationException {
		if (!token.contains(">") && !token.contains("row()")) {
			if (token.contains("<")) {
				throw new ConfigurationException("Triple element '" + token + "' has unmatched begin-variable delimiter ('<')");
			}
			else {
				throw new ConfigurationException("Triple element '" + token + "' does not seem to be a valid variable representation.");
			}
		}
		// this is a name containing a variable
		// this might be a name that already exists or it might be something new in this import namespace
		//  but we can't tell until we have replaced the named variable for a given row
		String pre = null;
		String post = null;
		String nm = null;
		if (!token.contains("<")) {
			int strt = token.indexOf("row()");
			pre = token.substring(0, strt);
			post = token.substring(strt + 5);
			nm = "row()";
		}
		else {
			pre = token.substring(0, token.indexOf("<"));
			post = token.substring(token.indexOf(">") + 1);
			nm = token.substring(token.indexOf("<") + 1, token.indexOf(">"));
		}
		if (varMap == null) {
			varMap = new HashMap<String, Object>();
		}

		Object var;
		if (!varMap.containsKey(token)) {
			if (pre.length() == 0 && post.length() == 0 && allDigits(nm)) {
				// this is a bnode reference by number only
				var = new Variable(null, null, null);
			}
			else {
				Object postObj;
				if (tokenContainsVariable(post)) {
					postObj = tokenToVariable(post);
				}
				else {
					postObj = post;
				}
				var = new Variable(pre, nm, postObj);
			}
			varMap.put(token, var);
		}
		else {
			var = varMap.get(token);
		}
		return var;
	}

	private Object tokenToConceptName(String token, int modelArrayPosition) throws ConfigurationException {
		if (token == null) {
			return null;
		}
		if (token.indexOf('^') >= 0) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < token.length(); i++) {
				if (token.charAt(i) != '^') {
					sb.append(token.charAt(i));
				}
			}
			token = sb.toString();
		}
		if (token.contains("#")) {
			ConceptName cn = getSadlUtils().getConceptByUri(getModel(modelArrayPosition), token);
			if (cn == null || cn.getType().equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
				throw new ConfigurationException("Completely qualified URI '" + token + "' not found in model.");
			}
			return cn;
		}
		else if (token.contains(":")) {
			String uri = getModel(modelArrayPosition).getNsPrefixURI(token.substring(0, token.indexOf(":")));
			if (uri == null) {
				uri = getConfigMgr().getUriFromGlobalPrefix(token.substring(0, token.indexOf(":")));
				if (uri != null) {
					if (!uri.endsWith("#")) {
						uri += "#";
					}
					getModel(modelArrayPosition).setNsPrefix(token.substring(0, token.indexOf(":")), uri);
				}
			}
			if (uri == null) {
				throw new ConfigurationException("Unable to resolve prefix '" + token.substring(0, token.indexOf(":")) + "'");
			}
			uri += token.substring(token.indexOf(":") + 1);
			ConceptName cn = getSadlUtils().getConceptByUri(getModel(modelArrayPosition), uri);
			if (cn ==  null || cn.getType().equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
				Property p = getModel(modelArrayPosition).getProperty(uri);
				if (p != null) {
					if (p.equals(RDFS.subClassOf)) {
						cn = new ConceptName(p.getLocalName());
						cn.setNamespace(p.getNameSpace());
						cn.setType(ConceptType.OBJECTPROPERTY);
						return cn;
					}
				}

				throw new ConfigurationException("Prefixed name '" + token + "' not found in model.");
			}
			return cn;
		}
		String uri = getModelNamespace() + token;
		ConceptName cn = getSadlUtils().getConceptByUri(getModel(modelArrayPosition), uri);
		if (cn != null && !cn.getType().equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
			return cn;
		}
		else if (imports != null && imports.length > 0){
			for (int i = 0; i < imports.length; i++) {
				String directImportNS = toNamespace(imports[i]);
				cn = findConceptInModel(modelArrayPosition, directImportNS, token);
				if (cn != null) {
					return cn;
				}
			}
			if (indirectImportNamespaces != null) {
				for (int i = 0; i < indirectImportNamespaces.size(); i++) {
					String indirectNS = toNamespace(indirectImportNamespaces.get(i));
					cn = findConceptInModel(modelArrayPosition, indirectNS, token);
					if (cn != null) {
						return cn;
					}
				}
			}
		}
		return token;
	}

	private ConceptName findConceptInModel(int modelArrayPosition, String ns, String ln) throws ConfigurationException {
		String uri = ns + ln;
		ConceptName cn = getSadlUtils().getConceptByUri(getModel(modelArrayPosition), uri);
		if (cn != null && !cn.getType().equals(ConceptType.CONCEPT_NOT_FOUND_IN_MODEL)) {
			if (cn.getPrefix() == null) {
				String prefix = getConfigMgr().getGlobalPrefix(stripNamespaceDelimiter(ns));
				if (prefix == null) {
					prefix = getModel(modelArrayPosition).getNsURIPrefix(ns);
				if (prefix == null) {
						prefix = getModel(modelArrayPosition).getNsURIPrefix(stripNamespaceDelimiter(ns));
					}
				}
				if (prefix != null) {
					getModel(modelArrayPosition).setNsPrefix(prefix, ns);
				}
				cn.setPrefix(prefix);
			}
			return cn;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#saveOwlModel(java.lang.String)
	 */
	@Override
	public boolean saveOwlModel(String saveAsFileName) throws ConfigurationException, IOException, InvalidNameException, ReasonerNotFoundException {
		setSaveAsFileName(saveAsFileName);
		boolean status;
		try {
			status = doImport(new InputStreamReader(csvInputStream));
		} catch (AbortDataRowException e1) {
			throw new IOException("Import failed: " + e1.getMessage(), e1);
		} catch (QueryCancelledException e1) {
			throw new IOException("Import failed: " + e1.getMessage(), e1);
		}
		if (status) {
			// after the model is built, save as OWL
			String fullyQualifiedOwlFilename = getSaveAsFileName();
			String format = "RDF/XML-ABBREV";
			File outFile = new File(fullyQualifiedOwlFilename);
			if (!outFile.exists()) {
				try {
					outFile.createNewFile();
				} catch (IOException e) {
					e.printStackTrace();
					throw new ConfigurationException("Unable to create output file '" + fullyQualifiedOwlFilename + "': " + e.getMessage());
				}
			}
			FileOutputStream fps = null;
			try {
				fps = new FileOutputStream(fullyQualifiedOwlFilename);
				if (fullyQualifiedOwlFilename.toLowerCase().endsWith(".owl")){
					format = "RDF/XML-ABBREV";
				}
				else if (fullyQualifiedOwlFilename.toLowerCase().endsWith(".n-triple")) {
					format = "N-TRIPLE";
				}
				else if (fullyQualifiedOwlFilename.toLowerCase().endsWith(".n3")) {
					format = "N3";
				}

				try {
					RDFWriter rdfw = getOwlModel().getWriter(format);
					// NTripleWriter.setProperty always throws UnknownPropertyException; ditto for N3.
					if (format.startsWith("RDF/XML")) {
						rdfw.setProperty("xmlbase", importModelNS); 
						rdfw.setProperty("relativeURIs", "");
						//			            rdfw.setProperty("minimalPrefixes", true);
					}
					OntModel om =  getOwlModel();
					om.setNsPrefix("", getModelNamespace());
					rdfw.write(om.getBaseModel(), fps, importModelNS);
					fps.close();
					IConfigurationManager cmgr = getConfigMgr();
					// TODO--fix this reduction of code
					//					if (cmgr instanceof ConfigurationManagerForIDE) {
					//				        try {
					//				        	((ConfigurationManagerForIDE)cmgr).addMapping(ResourceManager.fileNameToFileUrl(fullyQualifiedOwlFilename), importModelNS, null);
					//				        }
					//				        catch (Exception e) {
					//				        	throw new ConfigurationException("Failed to save mapping for model file '" + getSaveAsFileName() + "': " + e.getLocalizedMessage());
					//				        }
					//				        ((ConfigurationManagerForIDE)cmgr).replaceJenaModelCache(getModel(), importModelNS);
					//					}
				}
				catch (ConfigurationException e) {
					throw e;
				}
				catch (Throwable t) {
					t.printStackTrace();
					throw new ConfigurationException("Fatal error saving model file '" + importModelNS + "' to '" + getSaveAsFileName() + "': " + t.getLocalizedMessage());
				}
			} catch (FileNotFoundException e) {
				e.printStackTrace();
				throw new ConfigurationException("Unable to open output file '" + fullyQualifiedOwlFilename + "': " + e.getMessage());
			}
			finally {
				if (fps != null) {
					fps.close();
				}
			}
		}
		return status;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#saveSadlProjectOwlFileMapping(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public boolean saveSadlProjectOwlFileMapping(String publicUri, String actualUrl, String prefix) throws ConfigurationException {
		if (prefix == null) {
			prefix = importModelPrefix;
		}
		try {
			IConfigurationManager cmgr = getConfigMgr();
			if (cmgr instanceof ConfigurationManagerForEditing) {
				((ConfigurationManagerForEditing)cmgr).addMapping(getSadlUtils().fileNameToFileUrl(actualUrl), publicUri, prefix, false, CSV_IMPORTER);
//				((ConfigurationManagerForEditing)cmgr).saveOntPolicyFile();
			}
			else {
				String currentMapping = getConfigMgr().getAltUrlFromPublicUri(importModelNS);
				if ( currentMapping == null || !currentMapping.equals(actualUrl)) {
					ConfigurationManagerForEditing cfmfe = new ConfigurationManagerForEditing(modelFolderName, SadlSerializationFormat.JENA_TDB_FORMAT);
					cfmfe.addMapping(getSadlUtils().fileNameToFileUrl(actualUrl), publicUri, prefix, false, CSV_IMPORTER);
//					cfmfe.saveOntPolicyFile();
				}
			}
		}
		catch (Exception e) {
			throw new ConfigurationException("Failed to save mapping for model file '" + getSaveAsFileName() + "': " + e.getLocalizedMessage());
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#processImport()
	 */
	@Override
	public long processImport() throws ConfigurationException, IOException, InvalidNameException, AbortDataRowException, QueryCancelledException, ReasonerNotFoundException {
		boolean status = doImport(new InputStreamReader(csvInputStream));

		return numTriplesImported;

	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#getOwlModel(java.lang.String)
	 */
	@Override
	public DataSource getOwlModel(String format) throws ConfigurationException, IOException, InvalidNameException, ReasonerNotFoundException {
		format = validateFormat(format);
		try {
			boolean status = doImport(new InputStreamReader(csvInputStream));
		} catch (AbortDataRowException e) {
			throw new IOException("Import failed: " + e.getMessage(), e);
		} catch (QueryCancelledException e) {
			throw new IOException("Import failed: " + e.getMessage(), e);
		}
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		if (owlModelFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
			Model m = getModelFromTdbDS();
			m.write(out, format);
		}
		else {
			getModel(0).write(out, format);
		}
		String data = out.toString();
		StringDataSource ds = new StringDataSource(data, "text/plain");
		if (format.equals("RDF/XML") || format.equals("RDF/XML-ABBREV")) {
			ds.setName("OWL");
		}
		else if (format.equals("N-TRIPLE") ||format.equals("N3")) {
			ds.setName(format);
		}
		return ds;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#getOwlModel()
	 */
	@Override
	public OntModel getOwlModel() throws ConfigurationException, IOException, InvalidNameException, ReasonerNotFoundException {
		try {
			boolean status = doImport(new InputStreamReader(csvInputStream));
		} catch (AbortDataRowException e) {
			throw new IOException("Import failed: " + e.getMessage(), e);
		} catch (QueryCancelledException e) {
			throw new IOException("Import failed: " + e.getMessage(), e);
		}
		if (owlModelFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
			Model m = getModelFromTdbDS();
			return ModelFactory.createOntologyModel(getConfigMgr().getOntModelSpec(null), m);			
		}
		else {
			return getModel(0);
		}
	}

	private String validateFormat(String format) {
		if (SadlSerializationFormat.validateSadlFormat(format)) {
			return format;
		}
		else {
			return "RDF/XML-ABBREV";
		}
	}

	private boolean doImport(InputStreamReader isReader) throws ConfigurationException, IOException, InvalidNameException, AbortDataRowException, QueryCancelledException, ReasonerNotFoundException {
		try {
			if (processed ) {
				return true;
			}
			if (varMap == null) {
				throw new ConfigurationException("There are no variables found in the template; can't do import.");
			}
			// do the import
			//	open the CSV file with a CSV parser
			CSVReader reader = null;
			try {
				reader = new CSVReader(isReader);
	
				//	assume that the first row is the variable names (column headers)
				//	(if the variables used in the templates don't resolve and all
				//	 variable names are convertable to column designations by letter,
				// 	 reverse this assumption)
	
				int currentModel = 0;
	
				String [] nextLine;
				rowNum = 0;
				while ((nextLine = reader.readNext()) != null) {
					if (nextLine.length == 1 && nextLine[0].trim().length() == 0) {
						// an empty line in the data file
						continue;
					}
					// nextLine[] is an array of values from the line
					if (generatedValues != null) {
						generatedValues.clear();
					}
					if (transformedValues != null) {
						transformedValues.clear();
					}
					logger.debug("Processing CSV row " + rowNum);
					if (rowNum == 0 ) {
						int numMatching = 0;
						Iterator<String> varItr = varMap.keySet().iterator();
						while (varItr.hasNext()) {
							String key = varItr.next();
							if (varMap.get(key) instanceof Variable) {
								Variable var = (Variable) varMap.get(key);
								String varNm = var.name;
								if (varNm != null) {
									// this is NOT a bnode variable
									boolean found = false;
									if (varNm.equals("row()")) {
										continue;
									}
									boolean transformed;
									do {
										transformed = false;
										String baseName = getBaseName(varNm);
										if (transforms != null && transforms.containsKey(baseName)) {
											varNm = transforms.get(baseName).getInputIdentifier();
											transformed = true;
										}
									} while (transformed);
									if (includesHeader()) {
										for (int i = 0; i < nextLine.length; i++) {
											if (varNm.equals(nextLine[i])) {
												((Variable)var).setColumn(i);
												found = true;
												numMatching++;
												break;
											}
										}
									}
									if (!found || !includesHeader()) {
										int colNum = convertCharSeqToColumnNumber(varNm);
										if (colNum < 0) {
											throw new InvalidNameException("Variable name '" + varNm + "' was not found in header and as a column designation (" + colNum + ") is invalid.");											
										}
										if (colNum >= nextLine.length) {
											throw new InvalidNameException("Variable name '" + varNm + "' was not found in header and as a column designation (" + colNum + ") is larger than the number of columns in the input.");
										}
										var.setColumn(colNum);
									}
								}
							}
						}
					}
					if (!includesHeader() || rowNum > 0) {
						if (doTripleValidation(nextLine, rowNum, currentModel)) {
							// for the data rows (0- or 1-), apply the template to the values in 
							//	the row and add the triples to the model
							GroupOfTriples detectedGroup = null;
							if (groups != null && groups.containsKey(0) && groups.get(0).getEndingTriple() == templates.size() - 1) {
								// we have a group for this range of template triples; make sure it's active
								detectedGroup = groups.get(0);
								initializeGroupIndices(detectedGroup);
							}
							try {
								processGroup(detectedGroup, rowNum, nextLine, 0, templates.size() - 1, currentModel);
							} catch (SkipGroupException e) {
								rowNum++;
								continue;
							} catch (SkipTripleException e) {
								// this shouldn't happen
								e.printStackTrace();
								rowNum++;
								continue;
							}
						}
					}
					// if we are at the chunk size boundary, process the generated triples
					if (rowNum > 0 && chunkSize > 0 && rowNum%chunkSize == 0) {
						logger.debug("Ready to process chunk with model " + currentModel + " for rows " + (rowNum - chunkSize) + " to " + rowNum);
						processChunk(currentModel);
						currentModel += 1;
						// update the position in the array count when the rownum%chunksize == 0 and rownum > 0.
						// also, be sure to process the loop, so that the model number does not exceed the model count.
						if(currentModel >= numThreads){
							currentModel = 0;
						}
	
					}
					rowNum++;
				}
				if (rowNum > 0 && (chunkSize < 1 || (rowNum - 1)%chunkSize != 0)) {
					logger.debug("Ready to process final chunk with model " + currentModel + " for rows " + (rowNum - chunkSize) + " to " + rowNum);
					processChunk(currentModel);
				}
			}
			catch (Throwable t) {
				System.out.println("Throwable in doImport: " + t.getMessage());
				t.printStackTrace();
			}
			finally {
				try {
					executor.shutdown();
					while (!executor.awaitTermination(24, TimeUnit.HOURS)) {
						logger.debug("Waiting for all threads to complete before closing TDB Repo");
					}
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (triplesLoggerOut != null) {
					triplesLoggerOut.flush();
					triplesLoggerOut.close();
				}
				if (owlModelFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
					closeTdbDS();
				}
				if (reader != null) {
					reader.close();
				}
			}
		}
		finally {
			
		}
		processed = true;
		return true;
	}

	private void processChunk(int modelArrayPosition) throws ConfigurationException, ReasonerNotFoundException, IOException {
		logger.debug("Begin process chunk, global counter = " + modelsInUse);
		while (modelsInUse > 1 && modelsInUse >= numThreads) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		incrementModelsInUse();
		Runnable worker = new WorkerThread(models[modelArrayPosition], reasoners[modelArrayPosition], configMgr, modelArrayPosition, 
				infer, imports, owlModelFormat, getTdbDS(false));
		executor.execute(worker);
	}
	
	public synchronized void incrementModelsInUse() {
		modelsInUse ++;
	}
	
	public synchronized void decrementModelsInUse() {
		modelsInUse--;
	}	

	public static synchronized int convertCharSeqToColumnNumber(String ref) throws ConfigurationException {
		int pos = 0;
		int retval=0;
		for (int k = ref.length()-1; k >= 0; k--) {
			char thechar = ref.charAt(k);
			if (!Character.isLetter(thechar) || thechar < 65 || (thechar > 90 && thechar < 97) || thechar > 122) {
				throw new ConfigurationException("Name '" + ref + "' isn't found in headers (or headers not included) and isn't a valid column identifier, e.g. 'AZ'.");
			}
			// Character.getNumericValue() returns the values
			//  10-35 for the letter A-Z
			int shift = (int)Math.pow(26, pos);
			retval += (Character.getNumericValue(thechar)-9) * shift;
			pos++;
		}
		return retval-1;
	}

	private boolean doTripleValidation(String[] nextLine, int rowNum, int modelArrayPosition) throws AbortDataRowException {
		if (validates != null) {
			Iterator<String> vitr = validates.keySet().iterator();
			while (vitr.hasNext()) {
				Validate vdt = validates.get(vitr.next());
				if (vdt.getType().equals(Type.TriplePattern)) {
					Triple tr = vdt.getTriple();
					try {
						Property pred = useTemplateProperty(tr.predicate, nextLine, rowNum, modelArrayPosition);
						Resource subj;
						if (pred.equals(RDFS.subClassOf)) {
							subj = useTemplateResource(tr.subject, nextLine, pred, NewResourceType.Class, rowNum, modelArrayPosition);	
						}
						else {
							subj = useTemplateResource(tr.subject, nextLine, pred, NewResourceType.Individual, rowNum, modelArrayPosition);
						}
						RDFNode obj = useTemplateRDFNode(tr.object, nextLine, pred, rowNum, modelArrayPosition);
						if (subj != null && pred != null && obj != null) {
							if (!(obj instanceof Literal) || (obj instanceof Literal && ((Literal)obj).getValue() instanceof String)) {
								StmtIterator sitr = getModel(modelArrayPosition).listStatements(subj, pred, obj);
								if (sitr.hasNext()) {
									sitr.close();
									return true;
								}
							}
							else {
								// probably a number
								StmtIterator sitr = getModel(modelArrayPosition).listStatements(subj, pred, (RDFNode)null);
								while (sitr.hasNext()) {
									Statement stmt = sitr.nextStatement();
									Literal objValue = (Literal) stmt.getObject();
									if (ResultSet.valuesMatch(obj, objValue.getValue())) {
										return true;
									}
								}
							}
						}
					}
					catch (ReasonerNotFoundException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (QueryParseException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (QueryCancelledException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ConfigurationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvalidNameException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (SkipTripleException e) {
						logger.debug("Going to next row(" + rowNum + ") on skip triple: " + e.getMessage());
						continue;
					} catch (SkipGroupException e) {
						// this shouldn't happen
						e.printStackTrace();
						logger.debug("Going to next row(" + rowNum + ") on skip group: " + e.getMessage());
						continue;
					}

				}
			}
		}
		return true;
	}

	private void processGroup(GroupOfTriples thisGroup, int rowNum, String[] nextLine, int startTriple, int endTriple, int modelArrayPosition) throws ConfigurationException, InvalidNameException, AbortDataRowException, SkipGroupException, SkipTripleException  {
		// Bnode variables only need to be set once within a group
		activeGroup = thisGroup;
		do {
			setRowBnodeVariables(modelArrayPosition);
			// if no indexNames, do once. Otherwise do for all possible combinations of values for indices
			for (int i = startTriple; i <= endTriple; i++) {
				int grpEnd = i;
				do {
					i = grpEnd;
					try {
						grpEnd = processNestedGroups(thisGroup, rowNum, nextLine, i, modelArrayPosition);
					} catch (QueryCancelledException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				} while (grpEnd > i);
				if (i <= endTriple) {
					Triple tr = templates.get(i);
					Object skipCause = tr.predicate;
					try {
						Property pred = useTemplateProperty(tr.predicate, nextLine, rowNum, modelArrayPosition);
						skipCause = tr.subject;
						Resource subj;
						if (pred != null && pred.equals(RDFS.subClassOf)) {
							subj = useTemplateResource(tr.subject, nextLine, pred, NewResourceType.Class, rowNum, modelArrayPosition);	
						}
						else {
							subj = useTemplateResource(tr.subject, nextLine, pred, NewResourceType.Individual, rowNum, modelArrayPosition);
						}
						skipCause = tr.object;
						RDFNode obj = useTemplateRDFNode(tr.object, nextLine, pred, rowNum, modelArrayPosition);
						if (subj == null) {
							throw new ConfigurationException("Triple '" + tr.toString() + "' resolved to a null subject (row " + rowNum + ").");
						}
						else if (pred == null) {
							throw new ConfigurationException("Triple '" + tr.toString() + "' resolved to a null predicate (row " + rowNum + ").");
						}
						else if (obj == null) {
							throw new ConfigurationException("Triple '" + tr.toString() + "' resolved to a null object (row " + rowNum + "), predicate '" + 
									pred.getLocalName() + "' for thread " + modelArrayPosition + ".");
						}
						else {
							addTripleToModel(subj, pred, obj, rowNum, modelArrayPosition);
						}
					}
					catch (ReasonerNotFoundException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (QueryCancelledException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (QueryParseException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (SkipTripleException e) {
						if (triplesLoggerOut != null) {
							logSkip(rowNum, tr, skipCause, Disposition.SkipTriple);
						}
						continue;
					} catch (SkipGroupException e) {
						if (triplesLoggerOut != null) {
							logSkip(rowNum, tr, skipCause, Disposition.SkipGroup);
						}
						break;
					} catch (CircularDependencyException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}
			}
		} while (moreToDo());
	}

	/**
	 * Method to add a triple to the model, either in-memory or TDB repository
	 * @param subj
	 * @param pred
	 * @param obj
	 * @param rowNum
	 * @throws ConfigurationException 
	 * @throws CircularDependencyException 
	 */
	protected void addTripleToModel(Resource subj, Property pred, RDFNode obj,
			int rowNum, int modelArrayPosition) throws ConfigurationException, CircularDependencyException {
		if (pred.equals(RDF.type) && obj instanceof Resource) {
			// if we are assigning a type, look to see if it already has a more general type and if it does
			//	remove it.
			if (subj.canAs(Individual.class) && ((Resource)obj).canAs(OntClass.class)) {
				ExtendedIterator<Resource> typeitr = subj.as(Individual.class).listRDFTypes(true);
				while (typeitr.hasNext()) {
					Resource typ = typeitr.next();
					if (!typ.equals(obj) && typ.canAs(OntClass.class)) {
						if (SadlUtils.classIsSubclassOf(((Resource)obj).as(OntClass.class), typ.as(OntClass.class), true, null)) {
							subj.as(Individual.class).removeRDFType(typ.as(OntClass.class));
						}
					}
				}
			}
		}
		logger.debug("Adding triple while processing: " + subj + ", " + pred + ", " + obj);
		if (triplesLoggerOut != null) {
			logTriple(rowNum, subj, pred, obj, modelArrayPosition);
		}
		getModel(modelArrayPosition).add(subj, pred, obj);
//		getModel(modelArrayPosition).write(System.err, "N3");
		numTriplesImported++;
	}

	private void logTriple(int rowNum, Resource subj, Property pred, RDFNode obj, int modelArrayPosition) {
		try {
			triplesLoggerOut.write("" + (rowNum + 1));
			triplesLoggerOut.write(": ");
			triplesLoggerOut.write(getTripleElementAsString(subj, modelArrayPosition));
			triplesLoggerOut.write(", ");
			triplesLoggerOut.write(getTripleElementAsString(pred, modelArrayPosition));
			triplesLoggerOut.write(", ");
			triplesLoggerOut.write(getTripleElementAsString(obj, modelArrayPosition));
			triplesLoggerOut.write("\n");
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void logSkip(int rowNum, Triple tr, Object columnDesignator, Disposition disp) {
		try {
			String cd = (columnDesignator instanceof Variable) ? ((Variable)columnDesignator).toShortString() : columnDesignator.toString();
			triplesLoggerOut.write("" + (rowNum + 1));
			triplesLoggerOut.write(":      ");
			if (disp.equals(Disposition.SkipGroup)) {
				triplesLoggerOut.write("Skipping rest of group on blank for '" + cd + "' in triple '" + tr.toShortString());
			}
			else {
				triplesLoggerOut.write("Skipping triple on blank for '" + cd + "' in triple '" + tr.toShortString());
			}
			triplesLoggerOut.write("\n");
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#processNestedGroups(com.ge.research.sadl.importer.CsvImporter.GroupOfTriples, int, java.lang.String[], int, int)
	 */
	private int processNestedGroups(GroupOfTriples thisGroup, int rowNum,
			String[] nextLine, int currentTriple, int modelArrayPosition) throws ConfigurationException,
			InvalidNameException, AbortDataRowException, SkipTripleException, SkipGroupException, QueryCancelledException {
		if (groups != null && groups.containsKey(currentTriple)) {
			// we have a group starting here
			GroupOfTriples group = groups.get(currentTriple);
			if (!(group.equals(thisGroup))) {
				initializeGroupIndices(group);
				processGroup(group, rowNum, nextLine, currentTriple, group.getEndingTriple(), modelArrayPosition);
				activeGroup = thisGroup;	// restore
				currentTriple = group.getEndingTriple() + 1;	// we have already done the group; continue after
			}
		}
		return currentTriple;
	}

	/**
	 * This method tries to see if there is more to be done in this (the active) Group. Having more to do has to do with 
	 * array variables where the index is not specified so the processing automatically ranges over all possible values of 
	 * the index. If the index of an array variable is explicitly given, then there is no iteration to do--behavior is the
	 * same as for non-indexed variables.
	 * @return
	 * @throws AbortDataRowException
	 */
	private boolean moreToDo() throws AbortDataRowException {
		if (activeGroup == null || activeGroup.getIndices() == null) {
			return false;
		}
		if (activeGroup.getIndices().size() > 1) {
			throw new AbortDataRowException("Multiple indices within a group not yet handled.");
		}
		String nm = activeGroup.getIndices().keySet().iterator().next();
		GroupIndex gidx = activeGroup.getIndices().get(nm);
		gidx.setCurVal(gidx.getCurVal() + 1);
		if (gidx.getCurVal() <= gidx.getMaxVal()) {
			return true;
		}
		return false;
	}

	protected void initializeGroupIndices(GroupOfTriples group) {
		Map<String, GroupIndex> indices = group.getIndices();
		if (indices != null) {
			Iterator<String> indiceItr = indices.keySet().iterator();
			while (indiceItr.hasNext()) {
				String indexName = indiceItr.next();
				GroupIndex gidx = indices.get(indexName);
				gidx.setCurVal(0);
				gidx.setMaxVal(-1);
			}
		}
	}

	private void setRowBnodeVariables(int modelArrayPosition) throws ConfigurationException {
		Iterator<Object> vitr = varMap.values().iterator();
		while (vitr.hasNext()) {
			Object element = vitr.next();
			if (element instanceof Variable && ((Variable)element).name == null) {
				// this is a bnode variable
				if (((Variable)element).bnodeResource == null ||
						activeGroup == null || bnodeUsedOnlyInGroup(activeGroup, (Variable)element)) {
					((Variable)element).bnodeResource = getModel(modelArrayPosition).createResource();
				}
			}
		}

	}

	private boolean bnodeUsedOnlyInGroup(GroupOfTriples grp, Variable var) {
		int grpStart = grp.getStartingTriple();
		int grpEnd = grp.getEndingTriple();

		for (int i = 0; i < templates.size(); i++) {
			if (i < grpStart || i > grpEnd) {
				Triple tr = templates.get(i);
				if (tr.subject.equals(var) || tr.predicate.equals(var) || tr.object.equals(var)) {
					return false;
				}
			}
		}
		return true;
	}

	private boolean allDigits(String nm) {
		if (nm == null) {
			return false;
		}
		for (int i = 0; i < nm.length(); i++) {
			if (!Character.isDigit(nm.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	private RDFNode useTemplateRDFNode(Object object, String[] lineValues, Property predicate, int rowNum, int modelArrayPosition) throws ConfigurationException, InvalidNameException, AbortDataRowException, ReasonerNotFoundException, QueryParseException, SkipTripleException, SkipGroupException, QueryCancelledException {
		if (object instanceof ConceptName) {
			return conceptNameToOntResource(predicate, (ConceptName) object, modelArrayPosition);
		}
		else if (object instanceof Variable) {
			String token;
			if (((Variable)object).name == null) {
				// this is a bnode variable
				return ((Variable)object).bnodeResource;
			}
			else {
				token = variableToString((Variable)object, lineValues, rowNum, modelArrayPosition);
			}
			if (token == null || token.equals("\"\"") || token.length() < 1) {
				// warning?
				return null;
			}
			if (predicate.canAs(OntProperty.class) || predicate.equals(RDFS.subClassOf)) {
				OntProperty ontProp = predicate.equals(RDFS.subClassOf) ? null : predicate.as(OntProperty.class);
				if (predicate.equals(RDFS.subClassOf) || ontProp.isObjectProperty()) {
					Object expanded = tokenToConceptName(token, modelArrayPosition);
					if (expanded instanceof ConceptName) {
						Resource obj = getModel(modelArrayPosition).getIndividual(((ConceptName)expanded).getUri());
						if (obj == null) {
							// fail or create??
							obj = getModel(modelArrayPosition).createIndividual(((ConceptName)expanded).getUri(), null);
							//							throw new InvalidNameException("'" + object.toString() + "' should be an existing instance but not found in model.");
							return obj;
						}
						else {
							return obj;
						}
					}
					else if (expanded instanceof String) {
						//						expanded = ((String) expanded).trim().replace(' ', '_');	// first do spaces to make it more likely to be valid Xtext name
						//						expanded = URI.encode((String)expanded);
						String uri = getModelNamespace() + (String)expanded;
						Resource obj = getModel(modelArrayPosition).getIndividual(uri);
						if (obj == null) {
							obj = getModel(modelArrayPosition).createIndividual(uri, null); // createOntResource(uri);
						}
						return obj;
					}
					else {
						throw new InvalidNameException("'" + object.toString() + "' in row " + rowNum + " can't be resolved to an RDFNode.");
					}
				}
				else {
					try {
						return SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), ontProp, token);
					} catch (Exception e) {
						e.printStackTrace();
						throw new InvalidNameException("'" + object.toString() + "' in row " + rowNum + 
								" can't be assigned to property '" + ontProp.getLocalName() + 
								"' because an RDFNode of matching type could not be created: " + e.getMessage());
					}
				}
			}
			else if (predicate.equals(RDF.type)) {
				Object expanded = tokenToConceptName(token, modelArrayPosition);
				if (expanded instanceof ConceptName) {
					return getModel(modelArrayPosition).getOntClass(((ConceptName)expanded).getUri());
				}
				throw new InvalidNameException("'" + token + "' in row " + rowNum + " does not seem to be a valid class.");
			}
			else {
				StmtIterator stmtitr = getModel(modelArrayPosition).listStatements(predicate, RDFS.range, (RDFNode)null);
				if (stmtitr.hasNext()) {
					RDFNode obj = stmtitr.nextStatement().getObject();
					if (obj.isURIResource()) {
						String rnguri = obj.asResource().getURI();
						Literal lit;
						try {
							lit = SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), rnguri, token);
							if (lit != null) {
								return lit;
							}
						} catch (TranslationException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
				else if (predicate.getNameSpace().equals(OWL.getURI()) && 
						(predicate.getLocalName().equals(OWL.cardinality.getLocalName()) ||
								predicate.getLocalName().equals(OWL.maxCardinality.getLocalName()) ||
								predicate.getLocalName().equals(OWL.minCardinality.getLocalName()) ||
								predicate.getLocalName().equals(OWL2.qualifiedCardinality.getLocalName()) ||
								predicate.getLocalName().equals(OWL2.maxQualifiedCardinality.getLocalName()) ||
								predicate.getLocalName().equals(OWL2.minQualifiedCardinality.getLocalName()))) {
					Literal lit;
					try {
						lit = SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), XSD.xint.getURI(), token);
						if (lit != null) {
							return lit;
						}
					} catch (TranslationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
			return getModel(modelArrayPosition).createTypedLiteral(token);
		}
		else if (object instanceof String) {
			if (predicate.canAs(OntProperty.class) && predicate.as(OntProperty.class).isDatatypeProperty()) {
				try {
					return SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), predicate.as(OntProperty.class), object);
				} catch (TranslationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			else {
				Object objcn = tokenToConceptName((String) object, modelArrayPosition);
				String uri;
				if (objcn instanceof ConceptName) {
					uri = ((ConceptName)objcn).getUri();
				}
				else {
					uri = getModelNamespace() + (String)object;
				}
				Resource obj = getModel(modelArrayPosition).getIndividual(uri);
				if (obj == null) {
					obj = getModel(modelArrayPosition).createResource(uri);
				}
				return obj;
			}
		}
		else if (object instanceof Resource) {
			return (Resource)object;
		}
		else if (object instanceof Literal){
			return ((Literal)object);
		}
		throw new ConfigurationException("Triple object '" + object.toString() + "' in row " + rowNum + " isn't valid.");
	}

	private String variableToString(Variable var, String[] lineValues, int rowNum, int modelArrayPosition) throws InvalidNameException, AbortDataRowException, ConfigurationException, ReasonerNotFoundException, QueryParseException, SkipTripleException, SkipGroupException, QueryCancelledException {
		if (var.name.equals("row()")) {
			return var.prefix + (rowNum + 1) + postFixToString(var, lineValues, rowNum, modelArrayPosition);
		}
		else {
			return var.prefix + transformValue(var, lineValues[var.column].trim(), modelArrayPosition) + postFixToString(var, lineValues, rowNum, modelArrayPosition);
		}
	}

	private String postFixToString(Variable var, String[] lineValues, int rowNum, int modelArrayPosition) throws InvalidNameException, AbortDataRowException, ConfigurationException, ReasonerNotFoundException, QueryParseException, SkipTripleException, SkipGroupException, QueryCancelledException {
		Object pf = var.getPostfix();
		if (pf instanceof String) {
			return (String)pf;
		}
		else {
			if (pf instanceof Variable) {
				return variableToString((Variable)pf, lineValues, rowNum, modelArrayPosition);
			}
		}
		throw new InvalidNameException("postfix of Variable '" + var.toShortString() + "' is neither String nor Variable");
	}

	private Property conceptNameToProperty(ConceptName cn, int modelArrayPosition) throws InvalidNameException, ConfigurationException {
		return getModel(modelArrayPosition).getProperty(((ConceptName)cn).getUri());
	}

	private OntResource conceptNameToOntResource(ConceptName cn, int modelArrayPosition) throws InvalidNameException, ConfigurationException {
		return getModel(modelArrayPosition).getOntResource(cn.getUri());
	}

	private OntResource conceptNameToOntResource(Property predicate, ConceptName cn, int modelArrayPosition) throws InvalidNameException, ConfigurationException {
		if (predicate.equals(RDF.type) || 
				predicate.equals(RDFS.subClassOf) ||
				predicate.equals(RDFS.domain) ||
				predicate.equals(RDFS.range)) {
			OntResource or = getModel(modelArrayPosition).getOntClass(((ConceptName)cn).getUri());
			if (or == null) {
				if (cn.getNamespace().equals(OWL.getURI())) {
					Field fld;
					try {
						fld = OWL2.class.getDeclaredField(cn.getName());
						String nm = fld.getName();
						or = getModel(modelArrayPosition).createOntResource(OWL.getURI() + nm);
					} catch (NoSuchFieldException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (SecurityException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				else if (cn.getNamespace().equals(RDFS.getURI()) ) {
					Field fld;
					try {
						fld = RDFS.class.getDeclaredField(cn.getName());
						String nm = fld.getName();
						or = getModel(modelArrayPosition).createOntResource(RDFS.getURI() + nm);
					} catch (NoSuchFieldException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (SecurityException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				else if (cn.getNamespace().equals(RDF.getURI()) ) {
					Field fld;
					try {
						fld = RDF.class.getDeclaredField(cn.getName());
						String nm = fld.getName();
						or = getModel(modelArrayPosition).createOntResource(RDF.getURI() + nm);
					} catch (NoSuchFieldException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (SecurityException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
			return or;
		}
		else {
			return getModel(modelArrayPosition).getIndividual(((ConceptName)cn).getUri());
		}
	}

	private Property useTemplateProperty(Object predicate, String[] lineValues, int rowNum, int modelArrayPosition) throws ConfigurationException, InvalidNameException, AbortDataRowException, ReasonerNotFoundException, QueryParseException, SkipTripleException, SkipGroupException, QueryCancelledException {
		if (predicate instanceof ConceptName) {
			return getModel(modelArrayPosition).getProperty(((ConceptName)predicate).getUri());
		}
		else if (predicate instanceof Variable) {
			String token = variableToString((Variable)predicate, lineValues, rowNum, modelArrayPosition);
			Object expanded = tokenToConceptName(token, modelArrayPosition);
			if (expanded instanceof ConceptName) {
				return getModel(modelArrayPosition).getProperty(((ConceptName)expanded).getUri());
			}
			else if (expanded instanceof String) {
				String uri = getModelNamespace() + (String)expanded;
				Property pred = getModel(modelArrayPosition).getProperty(uri);
				if (pred == null) {
					pred = getModel(modelArrayPosition).createProperty(uri);
				}
				return pred;
			}
		}
		else if (predicate instanceof Property) {
			return (Property)predicate;
		}
		throw new ConfigurationException("Triple predicate '" + predicate.toString() + "' isn't valid.");
	}

	private Resource useTemplateResource(Object concept, String[] lineValues, Property pred, NewResourceType rtype, int rowNum, int modelArrayPosition) throws ConfigurationException, InvalidNameException, AbortDataRowException, ReasonerNotFoundException, QueryParseException, SkipTripleException, SkipGroupException, QueryCancelledException {
		if (concept instanceof ConceptName) {
			return getModel(modelArrayPosition).getIndividual(((ConceptName)concept).getUri());
		}
		else if (concept instanceof Variable) {
			String varName = ((Variable)concept).name;
			String token;
			if (varName == null) {
				// this is a bnode variable
				return ((Variable)concept).bnodeResource;
			}
			else {
				token = variableToString((Variable)concept, lineValues, rowNum, modelArrayPosition);
			}
			if (token == null || token.equals("\"\"") || token.length() < 1) {
				return null;
			}
			Object expanded = tokenToConceptName(token, modelArrayPosition);
			if (expanded instanceof ConceptName) {
				OntResource or = getModel(modelArrayPosition).getOntResource(((ConceptName)expanded).getUri());
				if (or != null && or.canAs(Individual.class)) {
					return or.as(Individual.class);
				}
				else {
					return or;
				}
				//				return getModel().getIndividual(((ConceptName)expanded).getUri());
			}
			else if (expanded instanceof String) {
				String uri = getModelNamespace() + (String)expanded;
				if (rtype.equals(NewResourceType.Individual)) {
					Resource rsrc = getModel(modelArrayPosition).getIndividual(uri);
					if (rsrc == null) {
						rsrc = getModel(modelArrayPosition).createIndividual(uri, null);
					}
					return rsrc;
				}
				else if (rtype.equals(NewResourceType.Class)) {
					Resource rsrc = getModel(modelArrayPosition).getOntClass(uri);
					if (rsrc == null) {
						rsrc = getModel(modelArrayPosition).createClass(uri);
					}
					return rsrc;
				}
				else {
					Resource rsrc = getModel(modelArrayPosition).getProperty(uri);
					if (rsrc == null) {
						rsrc = getModel(modelArrayPosition).createProperty(uri);
					}
					return rsrc;
				}
			}
		}
		else if (concept instanceof String) {
			String uri = getModelNamespace() + (String)concept;
			Resource rsrc = getModel(modelArrayPosition).getIndividual(uri);
			if (rsrc == null) {
				rsrc = getModel(modelArrayPosition).createResource(uri);
			}
			if (rsrc != null) {
				return rsrc;
			}
		}
		else if (concept instanceof Resource) {
			return (Resource)concept;
		}
		throw new InvalidNameException("Triple subject '" + concept.toString() + "' isn't valid.");
	}

	private Object[] validate(String varName, String value, int modelArrayPosition) throws AbortDataRowException, SkipTripleException, ConfigurationException, SkipGroupException {
		boolean isValid = true;
		if (validates.containsKey(varName) || validates.containsKey("all")) {
			Validate vdt = validates.get(varName);
			if (vdt == null) {
				vdt = validates.get("all");
			}
			if (vdt.getType().equals(Type.NonBlank)) {
				if (value == null || value.length() < 1) {
					if (vdt.disposition.equals(Disposition.Generate)) {
						if (getModelNamespace() != null) {
							if (generatedValues != null && generatedValues.containsKey(vdt.baseUri)) {
								value = generatedValues.get(vdt.baseUri);
							}
							else {
								String uri = getModelNamespace() + vdt.baseUri;
								value = SadlUtils.getUniqueOntUri(getModel(modelArrayPosition), uri);
								value = value.substring(value.lastIndexOf("#") + 1);
								if (generatedValues == null) {
									generatedValues = new HashMap<String, String>();
								}
								generatedValues.put(vdt.baseUri, value);
								logger.debug("validate not blank (" + varName + ") generate adding value '" + value + "'");
							}
						}
						else {
							throw new ConfigurationException("There is no model namespace--can't generate URI");
						}
					}
					else {
						generateExceptionOnInvalid(vdt.disposition, varName, "not blank");
					}
				}
			}
			else if (vdt.getType().equals(Type.ExistsInModel)) {
				Object cn;
				try {
					cn = tokenToConceptName(value, modelArrayPosition);
				} catch (ConfigurationException e) {
					throw new AbortDataRowException("Error converting '" + value + "' for variable '" + varName + "' in row " + rowNum + " to a concept name: " + e.getMessage());
				}
				if (!(cn instanceof ConceptName)) {
					logger.debug("validate (" + vdt.identifier + " did not find concept '" + value + "' in model, aborting.");
					throw new AbortDataRowException("Concept '" + value + "' for variable '" + varName + "' in row " + rowNum + " not found in model.");
				}
			}
			else if (vdt.getType().equals(Type.Decimal)) {
				try {
					double val = Double.parseDouble(value.trim());
				}
				catch (Exception e) {
					generateExceptionOnInvalid(vdt.disposition, varName, vdt.getType().toString());
				}
			}
			else if (vdt.getType().equals(Type.Integer)) {
				try {
					int val = Integer.parseInt(value.trim());
				}
				catch (Exception e) {
					generateExceptionOnInvalid(vdt.disposition, varName, vdt.getType().toString());
				}
			}
			else if (vdt.getType().equals(Type.Boolean)) {
				if (!(value.trim().equals("0") || value.trim().equals("1"))) {
					try {
						boolean val = Boolean.parseBoolean(value.trim());
					}
					catch (Exception e) {
						generateExceptionOnInvalid(vdt.disposition, varName, vdt.getType().toString());
					}
				}
			}
			else if (vdt.getType().equals(Type.Date)) {
				try {
					Literal val = SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), XSD.date.getURI(), value.trim());
					value = val.getValue().toString();
				}
				catch (Exception e) {
					generateExceptionOnInvalid(vdt.disposition, varName, vdt.getType().toString());
				}
			}
			else if (vdt.getType().equals(Type.DateTime)) {
				try {
					Literal val = SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), XSD.dateTime.getURI(), value.trim());
					value = val.getValue().toString();
				}
				catch (Exception e) {
					generateExceptionOnInvalid(vdt.disposition, varName, vdt.getType().toString());
				}
			}
			else if (vdt.getType().equals(Type.Time)) {
				try {
					Literal val = SadlUtils.getLiteralMatchingDataPropertyRange(getModel(modelArrayPosition), XSD.time.getURI(), value.trim());
					value = val.getValue().toString();
				}
				catch (Exception e) {
					generateExceptionOnInvalid(vdt.disposition, varName, vdt.getType().toString());
				}
			}
		}else {
			isValid = false;
		}
		Object[] results = new Object[2];
		results[0] = isValid;
		results[1] = value;
		return results;
	}

	private void generateExceptionOnInvalid(Disposition disposition, String varName, String condition) 
			throws AbortDataRowException, SkipTripleException, SkipGroupException {
		if (disposition.equals(Disposition.Abort)) {
			logger.debug("Validate " + condition + " (" + varName + ") failing so aborting.");
			throw new AbortDataRowException("Validate " + condition + " (" + varName + ") in row " + rowNum + " failing so aborting.");
		}
		else if (disposition.equals(Disposition.SkipTriple)) {
			logger.debug("Validate " + condition + " (" + varName + ") failing so skipping triple.");
			throw new SkipTripleException("Validate " + condition + " (" + varName + ") in row " + rowNum + " failing so skipping triple.");
		}
		else if (disposition.equals(Disposition.SkipGroup)) {
			logger.debug("Validate " + condition + " (" + varName + ") failing so skipping group.");
			throw new SkipGroupException("Validate " + condition + " (" + varName + ") in row " + rowNum + " failing so skipping group.");
		}
	}

	/**
	 * Method to check validity of value and transform if needed.
	 * 
	 * @param var
	 * @param trim
	 * @return
	 * @throws InvalidNameException 
	 * @throws AbortDataRowException 
	 * @throws ConfigurationException 
	 * @throws QueryParseException 
	 * @throws ReasonerNotFoundException 
	 * @throws SkipTripleException 
	 * @throws SkipGroupException 
	 * @throws QueryCancelledException 
	 */
	private String transformValue(Variable var, String value, int modelArrayPosition) throws InvalidNameException, AbortDataRowException, ConfigurationException, ReasonerNotFoundException, QueryParseException, SkipTripleException, SkipGroupException, QueryCancelledException {
		// check for validation or transformation on the variable
		String varName = var.name;
		String baseName = getBaseName(varName);
		boolean validated = false;
		if (transforms.containsKey(baseName)) {
			// here we need to follow the dependsOn chain to the bottom and transform values on the way back out to get the correct final value.
			//	awc 5/30/2014
			Transform tfrm = transforms.get(baseName);
			String transValue = validateAndTransform(tfrm, varName, validated, value, modelArrayPosition);
			Object[] validatedResult = validate(tfrm.getInputIdentifier(), transValue, modelArrayPosition);
			validated = (Boolean) validatedResult[0];
			transValue = (String) validatedResult[1];
			return transValue;
		}
		else {
			Object[] validatedResult = validate(varName, value, modelArrayPosition);
			validated = (Boolean) validatedResult[0];
			value = (String) validatedResult[1];
		}
		return value;
	}

	private String validateAndTransform(Transform tfrm, String varName, boolean validated, String value, int modelArrayPosition) throws AbortDataRowException, SkipTripleException, ConfigurationException, SkipGroupException, ReasonerNotFoundException, QueryParseException, QueryCancelledException, InvalidNameException {
		String transValue;
		Object val = value;
		if (tfrm.dependsOn != null) {
			val = validateAndTransform(tfrm.dependsOn, tfrm.getInputIdentifier(), validated, value, modelArrayPosition);
		}
		if (!validated) {
			Object[] validationResults = validate(tfrm.getInputIdentifier(), val.toString(), modelArrayPosition);
			validated = (Boolean) validationResults[0];
			value = (String) validationResults[1];
		}
		Object results = tfrm.transform(value, modelArrayPosition);
		if (results instanceof String[]) {
			String indexStr = getIndex(varName);
			if (indexStr != null) {
				if (allDigits(indexStr)) {
					int idx = Integer.parseInt(indexStr);
					if (idx <= ((String[])results).length) {
						transValue = ((String[])results)[idx - 1];
					}
					else {
						// check for skip validations
						validate(varName, null, modelArrayPosition);
						String values = "";
						for (int i = 0; i < ((String[])results).length; i++) {
							if (i > 0) {
								values += ", ";
							}
							values += ((String[])results)[i];
						}
						throw new InvalidNameException("Index (" + idx + ") of transformed variable '" + varName + "' in row " + rowNum + " is out of bounds (values are: " + values + ")");
					}
				}
				else {
					// so it must be a looping group index
					if (activeGroup != null) {
						if (activeGroup.getIndices() == null) {
							// it must be an implied index--use it and then remove it
							activeGroup.addIndex(indexStr);
						}
					
						if (activeGroup.getIndices() != null && activeGroup.getIndices().containsKey(indexStr)) {
							GroupIndex gidx = activeGroup.getIndices().get(indexStr);
							if (gidx.getMaxVal() < 0) {
								gidx.setCurVal(0);
								gidx.setMaxVal(((String[])results).length - 1);
							}
							else if (gidx.getMaxVal() != ((String[])results).length - 1) {
								throw new AbortDataRowException("Transform of '" + tfrm.getOutputIdentifier() + "' to '" + varName + "' in row " + rowNum + " did not produce the expected number of values for group indexing (produced " + 
										((String[])results).length + " but was expecting " + (gidx.getMaxVal() + 1) + ")");
							}
							return ((String[])results)[gidx.getCurVal()];
						}
						else {
							throw new InvalidNameException("Encountered variable '" + varName + " in row " + rowNum + "' which appears to be indexed but there is not a matching index on the active group.");
						}
					}
					else {
						throw new InvalidNameException("Encountered variable '" + varName + " in row " + rowNum + "' which appears to be indexed but there is not an active group.");
					}
				}
			}
			else {
				throw new InvalidNameException("No index found in transformed reference of '" + varName + "' in row " + rowNum);
			}
		}
		else if (results == null) {
			transValue = null;
		}
		else {
			transValue = results.toString();
		}
		return transValue;
	}

	@Override
	public String getModelNamespace() {
		return toNamespace(importModelNS);
	}
	
	@Override
	public String getModelName() {
		return stripSeparator(importModelNS);
	}

	public String stripSeparator(String ns) {
		if (ns.endsWith("#")) {
			return ns.substring(ns.length() - 1);
		}
		return ns;
	}


	private String toNamespace(String uri) {
		if (!uri.endsWith("#")) {
			return uri + "#";
		}
		return uri;		
	}

	private String validateFile(String fn, boolean isFolder) throws IOException {
		if (!fn.startsWith(HTTP_URI_SCHEME)) {
			if (fn.startsWith("file:/")) {
				fn = fn.substring(6);
			}
			File nf = new File(fn);
			if (!nf.exists()) {
				throw new IOException("'" + fn + "' does not exist.");
			}
			if (isFolder && !nf.isDirectory()) {
				throw new IOException("'" + fn + "' is not a folder as required.");
			}
		}
		return fn;
	}

	/**
	 * Parse an dateTime ("E MMM dd HH:mm:ss yyyy zzz") into
	 * a Calendar representation
	 * 
	 * @param timestamp
	 * @return
	 * @throws Exception
	 */
	public static synchronized Calendar parseTimestamp(String timestamp) throws Exception {
		/*
		 ** we specify Locale.US since months are in english
		 */
		SimpleDateFormat sdf = new SimpleDateFormat("E MMM dd HH:mm:ss yyyy zzz", Locale.US);
		Date d = sdf.parse(timestamp);
		Calendar cal = Calendar.getInstance();
		cal.setTime(d);
		return cal;
	}

	private OntModel getModel(int arraypos) throws ConfigurationException {
		// this method has been moved from getting the model singleton to getting the array specified.
		// hopefully, this will allow me to use multiple threads which each have their own associated model.
		if (models[arraypos] == null) {
			models[arraypos] = ModelFactory.createOntologyModel(getConfigMgr().getOntModelSpec(null));
			if (owlModelFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
				if (getTdbDS(false) == null) {
					tdbFolder = getSaveAsFileName();
					File f = new File(tdbFolder);
					boolean exists = f.exists();
					if (exists) {
						if (!f.isDirectory()) {
							throw new ConfigurationException("TDB folder '" + tdbFolder + "' already exists and is not a directory");
						}
						else if (!f.canWrite()) {
							throw new ConfigurationException("TDB folder '" + tdbFolder + "' isn't writable; could not delete.");
						}
					}
					else {
						f.mkdirs();
					}
					if (!incremental) {
						if (exists) {
							if (deleteTdbRepository(f)) {
								exists = false;
							}
						}
					}
					if (!exists) {
						if (imports != null && imports.length > 0) {
							addModelToTdbDS(getImportModel());
						}
					}
				}
			}
		}
		return models[arraypos];
	}

	private void clearModel(int arraypos) {
		models[arraypos] = null;
	}

	private IConfigurationManager getConfigMgr() throws ConfigurationException {
		if (configMgr == null) {
			if (modelFolderName == null) {
				throw new ConfigurationException("Model folder is not set.");
			}
			if (modelFolderName.startsWith(HTTP_URI_SCHEME)) {
				//				configMgr = new ConfigurationManager(modelFolderName);
				configMgr = ConfigurationManagerFactory.getConfigurationManager(modelFolderName, SadlSerializationFormat.RDF_XML_ABBREV_FORMAT);
			}
			// TODO resolve code removal
			else {
				String repoType;
				String tdbFolder = modelFolderName + "/TDB";
				String fname;
				try {
					fname = getSadlUtils().fileUrlToFileName(tdbFolder);
					File tdbFile = new File(fname);
					if (tdbFile.exists()) {
						repoType = SadlSerializationFormat.JENA_TDB_FORMAT;	
					}
					else {
						repoType = SadlSerializationFormat.RDF_XML_ABBREV_FORMAT;
					}
					configMgr = new ConfigurationManager(modelFolderName, repoType);
					SadlJenaModelGetterPutter modelGetter = new SadlJenaModelGetterPutter(configMgr, configMgr.getTdbFolder(), repoType);
					configMgr.setModelGetter(modelGetter);
				} catch (MalformedURLException e) {
					e.printStackTrace();
					throw new ConfigurationException("Failed to create ConfigurationManager: " + e.getMessage(), e);
				} catch (IOException e) {
					e.printStackTrace();
					throw new ConfigurationException("Failed to create ConfigurationManager: " + e.getMessage(), e);
				}
			}
		}
		return configMgr;
	}

	private boolean includesHeader() {
		return includesHeader;
	}

	private void setIncludesHeader(boolean includesHeader) {
		this.includesHeader = includesHeader;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#enableTriplesAddedInOrderLogging(java.lang.String)
	 */
	@Override
	public boolean enableTriplesAddedInOrderLogging(String filename) throws IOException {
		boolean status = false;
		if (allowTripleProcessingLog ) {
			if (filename.startsWith("\"")) {
				filename = SadlUtils.stripQuotes(filename);
			}
			triplesAddedInOrder = new File(filename);
			if (!triplesAddedInOrder.exists()) {
				try {
					status = triplesAddedInOrder.createNewFile();
				}
				catch (IOException e) {
					if (triplesAddedInOrder.getParentFile().mkdir()) {
						status = triplesAddedInOrder.createNewFile();
					}
					else {
						throw e;
					}
				}
			}
			else {
				if (triplesAddedInOrder.isFile() && triplesAddedInOrder.canWrite()) {
					triplesAddedInOrder.delete();
					status = triplesAddedInOrder.createNewFile();
				}
			}
			if (status) {
				triplesLoggerOut = new BufferedWriter(new FileWriter(triplesAddedInOrder));
			}
		}
		return status;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#getOwlModelFormat()
	 */
	@Override
	public String getOwlModelFormat() {
		return owlModelFormat;
	}

	/* (non-Javadoc)
	 * @see com.ge.research.sadl.importer.ICsvImporter#setOwlModelFormat(java.lang.String)
	 */
	@Override
	public void setOwlModelFormat(String owlModelFormat) throws InvalidNameException {
		if (!SadlSerializationFormat.validateSadlFormat(owlModelFormat)) {
			throw new InvalidNameException("Invalid OWL model format: " + owlModelFormat);
		}
		this.owlModelFormat = owlModelFormat;
	}

	/**
	 * Method to handle template file input by extracting uri and imports and returning template file content in 3 parts
	 * @param rawTemplate
	 * @return
	 * @throws TemplateException
	 * @throws IOException 
	 */
	private Object[] scanTemplateForNameAndImports(String rawTemplate) throws TemplateException, IOException {
		Object[] returnObject = new Object[3];
		StringBuffer templateSb = new StringBuffer();
		boolean usesCR = false;
		List<String> templateImports = new ArrayList<String>();
		try {
			Scanner s = new Scanner(rawTemplate);
			s.useDelimiter("\\n");
			while (s.hasNext()) {
				String templateLine = s.next();
				if (templateLine.endsWith("\r")) {
					templateLine = templateLine.substring(0,templateLine.length()-1);
					usesCR = true;
				}
				templateLine = dropEOS(templateLine);
//				if (templateLine.matches("\\s*uri\\s+\\S*\\s*")) {
				if (templateLine.matches("\\s*uri\\s+\\S*.*")) {
					String uri = templateLine.replaceFirst("\\s*uri\\s+","").trim();
					StringTokenizer st = new StringTokenizer(uri);
					uri = st.nextToken();
					if (st.hasMoreTokens()) {
						String alias = st.nextToken();
					}
					if (uri.startsWith("\"") && uri.endsWith("\"")) {
						uri = uri.substring(1, uri.length() - 1);
					}
					returnObject[0] = uri;
				}
				else if (templateLine.matches("\\s*import\\s+\\S*\\s*")) {
					String imp = templateLine.replaceFirst("\\s*import\\s+","").trim();
					if (imp.startsWith("\"") && imp.endsWith("\"")) {
						imp = imp.substring(1, imp.length() - 1);
					}
					templateImports.add(imp);
				} else if (templateLine.length() > 0) {
					templateSb.append(templateLine);
					if (usesCR) {
						templateSb.append("\r\n");
					} else {
						templateSb.append("\n");
					}
				}
			}
			s.close();
		} catch (Exception e) {
			e.printStackTrace();
			throw new TemplateException(e.getMessage());
		}
		returnObject[1] = templateImports;
		returnObject[2] = templateSb.toString();;
		return returnObject;
	}

	/**
	 * Method to drop a trailing period, if present, as it is the End of Statement.
	 * @param templateLine
	 * @return
	 */
	public static String dropEOS(String templateLine) {
		if (templateLine.trim().endsWith(".")) {
			int len = templateLine.trim().length();
			templateLine = templateLine.trim().substring(0, len - 1);
		}
		return templateLine;
	}

	public class WorkerThread implements Runnable {

		OntModel mod;
		IReasoner reas = null;
		IConfigurationManager icfg;
		int modelpos;
		boolean inferenceUsed;
		String[] importsList;
		String owlmodelFormatUsed;
		Dataset tdbDataSet;

		public WorkerThread(OntModel md, IReasoner raison, IConfigurationManager icfg_in, int modpos, boolean infer, String[] imports, String owlmod, Dataset ds) {
			mod = md;
			reas = raison;
			icfg = icfg_in;
			importsList = imports;
			owlmodelFormatUsed = owlmod;
			tdbDataSet = ds;
			inferenceUsed = infer;
			modelpos = modpos;
		}

		@Override
		public void run() {
			// perform the work originally in ProcessChunk(int)
			try{
				//System.out.println("started for model " + this.modelpos);
				if (inferenceUsed) {
					//System.out.println("inference turned on for thread " + this.modelpos);
					if (importsList == null) {
						throw new ConfigurationException(
								"Can't do inference without a schema model.");
					} else {
						if (importsList.length > 1) {
							throw new ConfigurationException(
									"Can't do inference with more than one import. Please create a single import model.");
						} 
						else {
							if (reas == null) {
								//System.out.println("getting reasoner for thread " + modelpos);
								reas = icfg.getCloneReasoner();

								int iStatus = reas.initializeReasoner(
										icfg.getModelFolder(), importsList[0],
										icfg.getModelGetter().getFormat());
								if (iStatus == 0) {
									logger.error("Status from initializeReasoner is " + iStatus);
								}
							}
							if (owlmodelFormatUsed
									.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
								reas.loadInstanceData(mod);
								Object imo = reas.getInferredModel(false);
								if (!(imo instanceof Model)) {
									throw new ConfigurationException("The reasoner returned an inferred model of type '" + imo.getClass().getCanonicalName() + "'; this is not supported by this importer.");
								}
								Model im = (Model) imo;
								StmtIterator sitr = im.listStatements(null, OWL.imports, (RDFNode)null);
								List<Statement> impstmts = new ArrayList<Statement>();
								while (sitr.hasNext()) {
									Statement stmt = sitr.nextStatement();
//									System.out.println(stmt);
									impstmts.add(stmt);
								}
								im.remove(impstmts);
								addModelToTdbDS(im);
								clearModel(modelpos);
								prepareNewModel(modelpos);
							} else {
								Object imo = reas.getInferredModel(false);
								if (!(imo instanceof Model)) {
									throw new ConfigurationException("The reasoner returned an inferred model of type '" + imo.getClass().getCanonicalName() + "'; this is not supported by this importer.");
								}
								Model im = (Model) imo;
								mod.add(im);
							}
							reas.reset(); // is this sufficient?
							logger.debug("Reasoner reset");
						}
					}
				}
				else {
					//System.out.println("inference turned off for thread " + this.modelpos);
					if (owlmodelFormatUsed.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
						addModelToTdbDS(mod);
						clearModel(modelpos);
						prepareNewModel(modelpos);
					}
				}
			}
			catch(Exception EEE){
				System.err.println("thread failed due to:");
				EEE.printStackTrace();
			}
			//System.out.println("ended for model " + this.modelpos);
			decrementModelsInUse();
		}

	}	

	/**
	 * Method to get the TDB DataSet. If it isn't set, or it has been closed (nulled), it cannot be created unless tdbFolder is set
	 * @return
	 * @throws ConfigurationException 
	 */
	private synchronized Dataset getTdbDS(boolean okToCreate) {
		if (tdbDS != null) {
			return tdbDS;
		}
		if (okToCreate && tdbFolder != null) {
			logger.debug("Creating new TDB Repo at '" + tdbFolder + "'");
			setTdbDS(TDBFactory.createDataset( tdbFolder ));
		}
		return tdbDS;
	}

	private void setTdbDS(Dataset tdbDS) {
		logger.debug("Setting importer TDB Dataset to " + tdbDS);
		this.tdbDS = tdbDS;
	}

	/**
	 * Method to add a model to the TDB repository with appropriate transaction control (in a thread-safe manner)
	 * @param m
	 * @return true if successful
	 */
	protected boolean addModelToTdbDS(Model m) {
		logger.debug("Adding a model to the TDB Repo Dataset " + tdbDS);
		getTdbDS(true).begin(ReadWrite.WRITE);
		tdbDS.getDefaultModel().add(m);
		tdbDS.commit();
		tdbDS.end();
		return true;
	}
	
	/** Method to get the default model from the TDB repository in a thread-safe manner
	 * 
	 * @return the default model
	 */
	protected Model getModelFromTdbDS() {
		logger.debug("Retrieving TDB Repo default model from Dataset " + tdbDS);
		Dataset ds = getTdbDS(false);
		getTdbDS(true).begin(ReadWrite.READ);
		Model m = tdbDS.getDefaultModel();
		tdbDS.end();
		if (ds == null) {
			closeTdbDS();
		}
		return m;
	}
	
	protected void closeTdbDS() {
		if (getTdbDS(false) != null) {
			while (tdbDS.isInTransaction()) {
				logger.debug("Waiting for TDB Repo transaction to complete before closing");
	//			thread.sleep(100);
			}
			logger.debug("Closing TDB Repo, setting Dataset " + tdbDS + " to null");
			TDB.sync(tdbDS);
			tdbDS.close();
			TDB.closedown();
			setTdbDS(null);
		}
	}
	
	/** Method to remove TDB repository from file system
	 * 
	 * @param f TDB folder on file system
	 * @return true if successful else false
	 */
	private synchronized boolean deleteTdbRepository(File f) {
		if (!getSadlUtils().recursiveDelete(f)) {
			TDB.closedown();
			if (!getSadlUtils().recursiveDelete(f)) {
				logger.debug("Failed to delete TDB Repo '" + f.getAbsolutePath() + "'");
				return false;
			}
		}
		logger.debug("Deleted TDB Repo '" + f.getAbsolutePath() + "'");
		return true;
	}


	private OntModel getImportModel() throws ConfigurationException {
		if (importModel == null) {
			importModel = ModelFactory.createOntologyModel(getConfigMgr().getOntModelSpec(null));
			importModel.setNsPrefix("rdf", RDF.getURI());
			importModel.setNsPrefix("rdfs", RDFS.getURI());
			importModel.setNsPrefix("owl", OWL.getURI());
			for (int i = 0; imports != null && i < imports.length; i++) {
				OntModel schemaModel = ModelFactory.createOntologyModel(getConfigMgr().getOntModelSpec(null));
				schemaModel.getDocumentManager().setProcessImports(true);
				schemaModel.read(getConfigMgr().getAltUrlFromPublicUri(imports[i]));
				Set<String> indirectImports = schemaModel.listImportedOntologyURIs(true);
				if (indirectImports != null) {
					Iterator<String> idiitr = indirectImports.iterator();
					while (idiitr.hasNext()) {
						String idins = idiitr.next();
						if (indirectImportNamespaces == null) {
							indirectImportNamespaces = new ArrayList<String>();
						}
						if (!indirectImportNamespaces.contains(idins)) {
							indirectImportNamespaces.add(idins);
						}
					}
				}
				importModel.add(schemaModel);
				String rawImport = imports[i];
				String altUrl = null;
				String publicUri = null;
				if (rawImport.toLowerCase().startsWith(HTTP_URI_SCHEME)) {
					altUrl = getConfigMgr().getAltUrlFromPublicUri(rawImport);
					publicUri = rawImport;
				}
				
				String prefix = getConfigMgr().getGlobalPrefix(publicUri);
				if (prefix != null) {
					importModel.setNsPrefix(prefix, toNamespace(publicUri));
				}
				importModel.getDocumentManager().addAltEntry(publicUri, altUrl);
			}
			StmtIterator sitr = importModel.listStatements((Resource)null, OWL.imports, (RDFNode)null);
			importModel.remove(sitr);	
		}
		return importModel;
	}

	private SadlUtils getSadlUtils() {
		if (sadlUtils  == null) {
			sadlUtils = new SadlUtils();
		}
		return sadlUtils;
	}

}
