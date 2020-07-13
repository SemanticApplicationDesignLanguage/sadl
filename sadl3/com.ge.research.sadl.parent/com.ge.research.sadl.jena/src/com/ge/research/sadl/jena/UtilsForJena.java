/************************************************************************
 * Copyright � 2007-2016 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.jena;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.utils.ResourceManager;
import org.apache.jena.ontology.CardinalityRestriction;
import org.apache.jena.ontology.MaxCardinalityRestriction;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.Restriction;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL2;

public class UtilsForJena {
	protected static final Logger logger = LoggerFactory.getLogger(UtilsForJena.class);

	public static final String OWL_MODELS_FOLDER_NAME = "OwlModels";
    public static final String ONT_POLICY_FILENAME = "ont-policy.rdf";
    
    public static final String FILE_SHORT_PREFIX = "file:/";
    public static final String FILE_URL_PREFIX = "file://";
    public static final String FILE_ABS_URL_PREFIX = "file:///";
    public static final String HTTP_URL_PREFIX = "http://";
    
	// Constants used to manage mappings between model namespace (publicURI) and model file (altURL) in ont-policy.rdf file
    public static final String SADL = "SADL";
    public static final String EXTERNAL_URL = "ExternalUrl";
	protected static final String OWL_ONT_MANAGER_PUBLIC_URINS = "http://www.w3.org/2002/07/owl";
	protected static final String ONT_MANAGER_LANGUAGE = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#language";
	protected static final String ONT_MANAGER_CREATED_BY = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#createdBy";
	protected static final String ONT_MANAGER_ALT_URL = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#altURL";
	protected static final String ONT_MANAGER_PUBLIC_URI = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#publicURI";
	protected static final String ONT_MANAGER_PREFIX = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#prefix";
	protected static final String ONT_MANAGER_ONTOLOGY_SPEC = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager#OntologySpec";


	protected RDFNode sadlNode = null;
	protected Property createdBy;
	protected Property altUrlProp;
	protected Property publicUrlProp;
	protected Property prefixProp;
	protected RDFNode createdBySadlLiteral;

	protected OntDocumentManager jenaDocumentMgr;
	private OntModelSpec ontModelSpec = null;
    
     /**
     * Call this method to remove double quotes from the beginning and end of a string so quoted.
     * @param quotedString -- the string from which quotes are to be removed
     */
    public static String stripQuotes(String quotedString) {
        if (quotedString != null && !quotedString.isEmpty()) {
            while (quotedString.charAt(0) == '\"') {
                quotedString = quotedString.substring(1);
            }
            while (quotedString.length() > 0 && quotedString.charAt(quotedString.length() - 1) == '\"') {
                quotedString = quotedString.substring(0, quotedString.length() - 1);
            }
        }
        return quotedString;
    }

	public static synchronized boolean isSingleValued(OntClass cls, OntProperty prop, String rngString) {
		if (prop.isFunctionalProperty()) {
			return true;
		}
		if (cls != null) {
			ExtendedIterator<OntClass> eitr = cls.listSuperClasses(false);
			while (eitr.hasNext()) {
				OntClass supercls = eitr.next();
				if (supercls.isRestriction()) {
					Restriction rstrct = supercls.asRestriction();
					if (rstrct.isMaxCardinalityRestriction()) {
						MaxCardinalityRestriction mxcr = rstrct.asMaxCardinalityRestriction();
						if (mxcr.getOnProperty().equals(prop) && mxcr.getMaxCardinality() == 1) {
							return true;
						}
					}
					else if (rstrct.isCardinalityRestriction()) {
						if (rstrct.isCardinalityRestriction()) {
							CardinalityRestriction cr = rstrct.asCardinalityRestriction();
							if (cr.getOnProperty().equals(prop) && cr.getCardinality() == 1) {
								return true;
							}
						}
					}
					else {
						if (rstrct.hasProperty(OWL2.maxQualifiedCardinality)) {
							if (rstrct.getOnProperty().equals(prop) && rstrct.getProperty(OWL2.maxQualifiedCardinality).getInt() == 1) {
								// check class
								if (rstrct.getProperty(OWL2.onClass).getResource().toString().equals(rngString)) {
									return true;
								}
							}
						}
						else if (rstrct.hasProperty(OWL2.qualifiedCardinality)) {
							if (rstrct.getOnProperty().equals(prop) && rstrct.getProperty(OWL2.qualifiedCardinality).getInt() == 1) {
								// check class
								if (rstrct.getProperty(OWL2.onClass).getResource().toString().equals(rngString)) {
									return true;
								}
							}							
						}
					}
				}
			}
		}
		return false;
	}
	
//	public String addMappingToPolicyFile(String content, String publicUri, String altUrl, String globalAlias, String source) throws JenaProcessorException {
//		// read content into a Model
//        Model m = ModelFactory.createDefaultModel();
//        m.read(new ByteArrayInputStream(content.getBytes()), null);
//        
//        // add/update the model with the specified mapping
//        initializePolicyConcepts(m);
//		Resource pubv = m.createResource(publicUri);
//		Resource altv = m.createResource(altUrl);
//		Literal pref = null;
//		if (globalAlias != null) {
//			pref = m.createTypedLiteral(globalAlias);
//		}
//		addMapping(m, altv, pubv, pref, false, source);
//        
//        // prepare the new content and return it
//		String pfBase = "http://jena.hpl.hp.com/schemas/2003/03/ont-manager";
//		String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
//		RDFWriter w = m.getWriter(format);
//		w.setProperty("xmlbase", pfBase);
//		ByteArrayOutputStream out = new ByteArrayOutputStream();
//		w.write(m, out, pfBase);
//		Charset charset = Charset.forName("UTF-8"); 
//		CharSequence seq = new String(out.toByteArray(), charset);
//		return seq.toString();
//	}

//	public String getMinimalPolicyFileContent() throws IOException, URISyntaxException {
//        File source = ResourceManager.getAbsoluteBundlePath("Models", ONT_POLICY_FILENAME);
//        return new SadlUtils().fileToString(source);
//		StringBuilder sb = new StringBuilder();
//		sb.append("<rdf:RDF\n");
//		sb.append("xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n");
//		sb.append("xmlns=\"http://jena.hpl.hp.com/schemas/2003/03/ont-manager#\"\n");
//		sb.append("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\">\n");
//		sb.append("<OntologySpec>\n");
//		sb.append("<language rdf:resource=\"http://www.w3.org/2002/07/owl\"/>\n");
//		sb.append("<publicURI rdf:resource=\"http://www.w3.org/2002/07/owl\"/>\n");
//		sb.append("<prefix rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\"\n");
//		sb.append(">owl</prefix>\n");
//		sb.append("</OntologySpec>\n");
//		sb.append("<DocumentManagerPolicy>\n");
//		sb.append("  <cacheModels rdf:datatype=\"http://www.w3.org/2001/XMLSchema#boolean\"\n");
//		sb.append("  >true</cacheModels>\n");
//		sb.append("  <processImports rdf:datatype=\"http://www.w3.org/2001/XMLSchema#boolean\"\n");
//		sb.append("  >true</processImports>\n");
//		sb.append("</DocumentManagerPolicy>\n");
//		sb.append("<OntologySpec>\n");
//		sb.append("  <altURL rdf:resource=\"http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl\"/>\n");
//		sb.append("  <publicURI rdf:resource=\"http://purl.org/dc/elements/1.1/\"/>\n");
//		sb.append("  <language rdf:resource=\"http://www.w3.org/2002/07/owl\"/>\n");
//		sb.append("  <prefix rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\"\n");
//		sb.append("  >dc</prefix>\n");
//		sb.append("</OntologySpec>\n");
//		sb.append("</rdf:RDF>\n");
//		return sb.toString();
//	}

//	private boolean initializePolicyConcepts(Model m) {
//    	if (sadlNode == null) {
//    		sadlNode = m.createTypedLiteral(SADL);
//    		createdBy = m.createProperty(ONT_MANAGER_CREATED_BY);
//    		altUrlProp = m.createProperty(ONT_MANAGER_ALT_URL);
//    		publicUrlProp = m.createProperty(ONT_MANAGER_PUBLIC_URI);
//    		prefixProp = m.createProperty(ONT_MANAGER_PREFIX);
//    		return true;
//    	}
//    	return false;
//	}

//	public synchronized boolean addMapping(Model m, Resource altv, Resource pubv, Literal prefix, boolean bKeepPrefix, String source) {
//		boolean bChanged = false;
//		boolean mappingFound = false;
//		List<Statement> pendingDeletions = null;
//		// Get all the statements that have this public URI
//		StmtIterator pubitr = m.listStatements(null,
//				publicUrlProp, pubv);
//		if (pubitr.hasNext()) {
//			mappingFound = true;
//			int cntr = 0;
//			while (pubitr.hasNext()) {
//				Statement s = pubitr.nextStatement();
//				if (cntr > 0) {
//					// there are multiple entries for this public URI
//					if (pendingDeletions == null) {
//						pendingDeletions = new ArrayList<Statement>();
//					}
//					pendingDeletions.add(s);
//				} else {
//					Resource subj = s.getSubject();
//					// find the corresponding altURL
//					Statement s2 = subj.getProperty(altUrlProp);
//					if (s2 != null) {
//						// Is the old and the new actual URL the same? If not
//						// then change the statement for the actual URL
//						if (!s2.getObject().equals(altv)) {
//							if (pendingDeletions == null) {
//								pendingDeletions = new ArrayList<Statement>();
//							}
//							pendingDeletions.add(s2);
//							subj.addProperty(altUrlProp, altv);
//							bChanged = true;
//						}
//					} else {
//						subj.addProperty(altUrlProp, altv);
//						bChanged = true;
//					}
//					Statement s3 = subj.getProperty(prefixProp);
//					if (s3 != null) {
//						// there is already a prefix in the model
//						if (prefix != null) {
//							// we have another which is not null
//							if (!s3.getObject().equals(prefix)) {
//								if (!bKeepPrefix) {
//									// only make the change if not keeping old prefix (when the new prefix is null)
//									if (pendingDeletions == null) {
//										pendingDeletions = new ArrayList<Statement>();
//									}
//									pendingDeletions.add(s3);
//								}
//								if (prefix != null) {
//									subj.addProperty(prefixProp, prefix);
//								}
//								bChanged = true;
//							}
//						}
//					} else if (prefix != null) {
//						subj.addProperty(prefixProp, prefix);
//						bChanged = true;
//					}
//				}
//				cntr++;
//			}
//		}
//		StmtIterator altitr = m.listStatements(null,
//				altUrlProp, altv);
//		if (altitr.hasNext()) {
//			mappingFound = true;
//			int cntr = 0;
//			while (altitr.hasNext()) {
//				Statement s = altitr.nextStatement();
//				if (cntr > 0) {
//					// there are mulitiple statements for this alt URL
//					if (pendingDeletions == null) {
//						pendingDeletions = new ArrayList<Statement>();
//					}
//					pendingDeletions.add(s);
//				} else {
//					if (!bChanged) {
//						// if bChanged is true then we must have already fixed
//						// the one mapping in the section above--no need to do
//						// it again
//						Resource subj = s.getSubject();
//						// find the corresponding publicUri
//						Statement s2 = subj.getProperty(publicUrlProp);
//						if (s2 != null) {
//							// is the old and the new public URI the same? If
//							// not then change the statement for the new public
//							// URI
//							if (!s2.getObject().equals(pubv)) {
//								if (pendingDeletions == null) {
//									pendingDeletions = new ArrayList<Statement>();
//								}
//								pendingDeletions.add(s2);
//								subj.addProperty(publicUrlProp, pubv);
//								bChanged = true;
//							}
//						}
//						subj.addProperty(publicUrlProp, pubv);
//						bChanged = true;
//
//						Statement s3 = subj.getProperty(prefixProp);
//						if (s3 != null) {
//							// there is already a prefix in the model
//							if (prefix != null) {
//								// we have another which is not null
//								if (!s3.getObject().equals(prefix)) {
//									if (!bKeepPrefix) {
//										// only make the change if not keeping old prefix (when the new prefix is null)
//										if (pendingDeletions == null) {
//											pendingDeletions = new ArrayList<Statement>();
//										}
//										pendingDeletions.add(s3);
//									}
//									if (prefix != null) {
//										subj.addProperty(prefixProp, prefix);
//									}
//									bChanged = true;
//								}
//							}
//						} else if (prefix != null) {
//							subj.addProperty(prefixProp, prefix);
//							bChanged = true;
//						}
//					}
//				}
//				cntr++;
//			}
//		}
//
//		// remove extra and obsolete entries
//		if (pendingDeletions != null && pendingDeletions.size() > 0) {
//			for (int i = 0; i < pendingDeletions.size(); i++) {
//				Statement s = pendingDeletions.get(i);
//				m.remove(s);
//				bChanged = true;
//			}
//		}
//
//		if (!mappingFound) {
//			org.apache.jena.rdf.model.Resource type = m
//					.createResource(ONT_MANAGER_ONTOLOGY_SPEC);
//			org.apache.jena.rdf.model.Resource newOntSpec = m
//					.createResource(type);
//			Property langp = m
//					.getProperty(ONT_MANAGER_LANGUAGE);
//			RDFNode langv = m.createResource(
//					OWL_ONT_MANAGER_PUBLIC_URINS);
//			m.add(newOntSpec, publicUrlProp, pubv);
//			m.add(newOntSpec, altUrlProp, altv);
//			m.add(newOntSpec, langp, langv);
//			if (source != null && !source.equalsIgnoreCase(SADL)) {
//				m.add(newOntSpec, createdBy, m.createTypedLiteral(source));
//			} else {
//				m.add(newOntSpec, createdBy, SADL);
//			}
//			if (prefix != null) {
//				m.add(newOntSpec, prefixProp, prefix);
//			}
//			logger.debug("Created new mapping for '" + pubv.toString() + "', '"
//					+ altv.toString() + "'");
//			bChanged = true;
//		}
//		try {
//			// add mapping to Jena OntDocumentManager
//			if (addJenaMapping(pubv.getURI().toString(), altv.getURI()
//					.toString())) {
//				bChanged = true;
//			}
//		} catch (URISyntaxException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (ConfigurationException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		if (bChanged) {
//			setMappingChanged(true);
//			logger.debug("Modified mapping for '" + pubv.toString() + "', '"
//					+ altv.toString() + "'");
//		}
//		if (this.mappings == null) {
//			mappings = new HashMap<String, String>();
//		}
//		mappings.put(rdfNodeToString(pubv), rdfNodeToString(altv));
//		return bChanged;
//	}
	
	public static String getPolicyFilePathForProject(String projectPath) throws ConfigurationException {
		if (projectPath.startsWith("file:/")) {
			projectPath = projectPath.substring(6);
		}
		else if (projectPath.startsWith("file:")) {
			projectPath = projectPath.substring(5);
		}
		File prjFile = new File(projectPath);
		if (!prjFile.exists()) {
			throw new ConfigurationException("Project path '" + projectPath + "' does not exist.");
		}
		if (!prjFile.isDirectory()) {
			throw new ConfigurationException("Project path '" + projectPath + "' is not a folder.");
		}
     	return projectPath + "/" + OWL_MODELS_FOLDER_NAME + "/" + ONT_POLICY_FILENAME;
	}

//	public OntDocumentManager loadMappings(File pf) throws IOException {
//		if (!pf.exists()) {
//			// reconstruct the policy file, including externals
//		}
//		else if (!pf.isFile()) {
//			throw new IOException("'" + pf.getCanonicalPath() + "' is not a valid policy file identifier");
//		}
//		// load mapping info from file
//		Model mappingModel = ModelFactory.createDefaultModel();
//	    InputStream in = FileManager.get().open(pf.getCanonicalPath());
//	    if (in == null) {
//	    	throw new IllegalArgumentException("File: " + pf.getCanonicalPath() + " not found");
//	    }
//		try {
//			mappingModel.read(in, "");
//		}
//		catch (Throwable t) {
//			t.printStackTrace();
//			logger.error("Failed to read mapping file '" + pf.getCanonicalPath() + "': " + t.getLocalizedMessage());
//		}
//		
//		boolean needFileLocator = false;
//        RDFNode pubv;
//        RDFNode altv;
////        createdBySadlLiteral = mappingModel.createLiteral("SADL");
//        String fileName = new String();
//        String actualFilePath = new String();
//        initializePolicyConcepts(mappingModel);
//        StmtIterator sitr = mappingModel.listStatements(null, altUrlProp, (RDFNode)null);
//        
//        while (sitr.hasNext()) {
//        	fileName = null;
//        	actualFilePath = null;
//            Statement s = sitr.nextStatement();
//            org.apache.jena.rdf.model.Resource subj = s.getSubject();	
//            Statement salt = subj.getProperty(altUrlProp);
//            Statement spub = subj.getProperty(publicUrlProp);
//            Statement sprefix = subj.getProperty(prefixProp);
//            if (salt != null && spub != null) {
//	            altv = salt.getObject();
//            	String strAltv = rdfNodeToString(altv);
//	            pubv = spub.getObject();
//	            Statement isSadlStmt = subj.getProperty(createdBy);
//	            if (isSadlStmt != null && isSadlStmt.getObject().asLiteral().getLexicalForm().equals(sadlNode.asLiteral().getLexicalForm())) {         
//	            	// this mapping was created by SADL
//		            StringTokenizer st1 = new StringTokenizer(strAltv, "/");
//		 			while(st1.hasMoreTokens()) {
//		            	fileName = st1.nextToken();
//		 			}
//	            	actualFilePath = getActualUrl(pf.getParent(), fileName);
//	            }
//	            else {
//	            	// this handles mappings that are not created by SADL
//	            	//  1) if the mapping is of type "file:" and the file exists assume it is correct
//	            	//	2) else if their is a file of that name in the same folder as the policy file assume that file is the correct one
//	            	//	3) else if there is a sibling folder to the folder of the policy file that contains a file of that name assume it is the correct one
//	            	if (strAltv.startsWith(FILE_SHORT_PREFIX)) {
//			            StringTokenizer st1 = new StringTokenizer(strAltv, "/");
//			            String lastToken = null;
//			 			while(st1.hasMoreTokens()) {
//			 				lastToken = fileName;
//			            	fileName = st1.nextToken();
//			 			}
//			 			String testName = strAltv;
//		 				try {
//				 			File testFile = new File(fileUrlToFileName(testName));
//				 			if (testFile.exists()) {
//				 				// the actualUrl exists as is so use it
//				 				actualFilePath = testName;
//				 			}
//				 			else {
//				 				testName =  getActualUrl(pf.getParent(), fileName);
//								testFile = new File(fileUrlToFileName(testName));
//				 				if (testFile.exists()) {
//				 					// the actualUrl adjusted to have the relative location of the models folder exists so use it
//				 					actualFilePath = testName;
//				 				}
//				 				else {
//				 					String siblingName = siblingFolderUrl(pf.getParent(), fileName, lastToken);
//				 					boolean siblingFound = false;
//				 					if (siblingName != null) {
//				 						File sibling = new File(fileUrlToFileName(siblingName));
//				 						if (sibling.exists()) {
//				 							// the named file exists in a sibling directory; use it
//				 							siblingFound = true;
//				 							actualFilePath = siblingName;
//				 						}
//				 					}
//				 					if (!siblingFound) {
//				 						if (pf.getParent() != null) {
//								 			String folderPath = fileNameToFileUrl(pf.getParent());
//						 					testName = folderPath.substring(0, folderPath.length() - (1 + lastToken.length())) + "/" + fileName;
//						 					testFile = new File(fileUrlToFileName(testName));
//						 					if (testFile.exists()) {
//						 						// folder above??
//						 						actualFilePath = testName;
//						 					}
//						 					else {
//						 						logger.warn("Mapping file has actual URL '" + testName + "' but it does not appear to exist and could not be found in adjacent folders.");
//						 					}
//				 						}
//				 						else {
//				 							actualFilePath = testName;
//				 							if (!actualFilePath.startsWith("http:")) {
//				 								logger.warn("Mapping file '" + strAltv + "'; using '" + actualFilePath + "'");
//				 							}
//				 						}
//				 					}
//				 				}
//				 			}
//		 				}
//						catch (MalformedURLException e) {
//							// oh well, we tried
//						}
//	            	}
//	            }
//	            if (actualFilePath == null) {
//	            	actualFilePath = strAltv;
//	            }
//	            String publicUri = rdfNodeToString(pubv);
//	            logger.debug("Found mapping from public URI '" + publicUri + "' to alternative URL '" + actualFilePath + "'");
//	            if (!actualFilePath.equals(publicUri)) {
//	            	getJenaDocumentMgr(mappingModel).addAltEntry(publicUri, actualFilePath);
//					if (actualFilePath != null && actualFilePath.startsWith(FILE_SHORT_PREFIX)) {
//						needFileLocator = true;
//					}
//	            }
//           }
//            
//         } // end while
//		
//		if (needFileLocator) {
//			setupJenaFileManager(pf.getParent(), mappingModel);
//		}
//		return getJenaDocumentMgr(mappingModel);
//	}

    protected String rdfNodeToString(RDFNode node) {
    	if (node != null) {
			if (node instanceof Literal) {
				return ((Literal)node).getValue().toString();
			}
			return node.toString();
    	}
    	return null;
	}

//	private String getActualUrl(String modelFolder, String fileName) {
//		String rawPath = modelFolder + "/" + fileName;
//		return fileNameToFileUrl(rawPath);
//	}
//
	/**
     * This method converts an OS filename (e.g., "C:\\folder\file.ext")
     * to a file URL
     *
     * @param fileName
     * @return
     */
    public String fileNameToFileUrl(String fileName) {
    	URI fileUri = null;
        if (fileName.startsWith("http:") || fileName.startsWith("file:")) {
            fileUri = URI.create(fileName);
        }
        else {
        	File file = new File(fileName);
        	fileUri = file.toURI();
        	try {
				return fileUri.toURL().toExternalForm();
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return FILE_ABS_URL_PREFIX + fileName.replace("\\", "/");
			}
        }
        return fileUri.toString();
    }

    /**
     * This method converts the string form of a file URL to an
     * OS filename
     *
     * @param urlstr
     * @return
     * @throws MalformedURLException 
     */
    public String fileUrlToFileName(String urlstr) throws MalformedURLException {
    	if (urlstr.startsWith(FILE_URL_PREFIX)) {
	        URI fileUri = URI.create(urlstr);
	        if (fileUri != null) {
	        	return fileUri.toURL().getPath();
	        }
	        throw new MalformedURLException("Unable to convert '" + urlstr + "' to a file name");
    	}
    	return urlstr;
    }

//	private String siblingFolderUrl(String modelFolder, String fileName, String lastToken) {
//		if (modelFolder != null) {
//			File folder = new File(modelFolder);
//			File parent = folder.getParentFile();
//			File[] siblings = parent.listFiles();
//			for (int i = 0; i < siblings.length; i++) {
//				if (siblings[i].getName().equals(lastToken)) {
//					return fileNameToFileUrl(siblings[i].getAbsolutePath() + File.separator + fileName);
//				}
//			}
//		}
//		return null;
//	}
//	
//	private void setupJenaFileManager(String modelFolder, Model mappingModel) throws IOException {
//		getJenaDocumentMgr(mappingModel).getFileManager().addLocatorFile(modelFolder);
//		getJenaDocumentMgr(mappingModel).getFileManager().addLocatorURL();
//		SadlReadFailureHandler rfHandler = new SadlReadFailureHandler(logger );	
////		rfHandler.setSadlConfigMgr(this);
//		getJenaDocumentMgr(mappingModel).setReadFailureHandler(rfHandler);
//	}
//	
//	public OntDocumentManager getJenaDocumentMgr(Model mappingModel) {
//		if (jenaDocumentMgr == null) {
//			if (mappingModel != null) {
//				setJenaDocumentMgr(new OntDocumentManager());
//				if (ontModelSpec != null) {
//					ontModelSpec.setDocumentManager(getJenaDocumentMgr(mappingModel));
//				}
//			}
//			else {
//				setJenaDocumentMgr(OntDocumentManager.getInstance());
//			}
//		}
//		return jenaDocumentMgr;
//	}
//
//	private void setJenaDocumentMgr(OntDocumentManager docmgr) {
//		jenaDocumentMgr = docmgr;
//	}
	
	public String getPolicyFilename(org.eclipse.emf.ecore.resource.Resource somerojectResource) {
		org.eclipse.emf.common.util.URI prjUri = ResourceManager.getProjectUri(somerojectResource);
		if (prjUri != null) {
			org.eclipse.emf.common.util.URI uri = prjUri.appendSegment(OWL_MODELS_FOLDER_NAME);
			uri = uri.appendSegment(ONT_POLICY_FILENAME);
			if (uri.isPlatform()) {
				 IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(uri.toPlatformString(true)));
				 return file.getRawLocation().toPortableString();
			}
			else {
				return uri.toFileString();
			}
		}
		return null;
	}

//	public String getNewPolicyFileContent(String policyFilename) throws IOException, URISyntaxException, JenaProcessorException {
//		StringBuilder sb = new StringBuilder(getMinimalPolicyFileContent());
//		sb = addExternalMappings(sb, new File(policyFilename).getParentFile().getParent());
//		return sb.toString();
//	}
	
//	public synchronized OntModel createAndInitializeJenaModel(String policyFilename, OntModelSpec omSpec, boolean loadImports) 
//			throws IOException, ConfigurationException, URISyntaxException, JenaProcessorException {
//		File pf = null;
//		if (policyFilename != null && policyFilename.length() > 0) {
//			pf = new File(policyFilename);
//			if (!pf.exists()) {
//				String pfContent = getNewPolicyFileContent(policyFilename);
//				File pfp = pf.getParentFile();
//				pfp.mkdirs();
//				new SadlUtils().stringToFile(pf, pfContent, false);
//			}
//		}
//		else {
//			logger.warn("Policy file name is invalid");
//		}
//		if (pf != null) {
//			String modelFolder = pf.getParent();
//			OntDocumentManager owlDocMgr = loadMappings(pf);
//			OntModelSpec spec = new OntModelSpec(omSpec);
//			spec.setImportModelGetter(new SadlJenaModelGetterPutter(spec, modelFolder));
//			spec.setDocumentManager(owlDocMgr);
//			owlDocMgr.setProcessImports(loadImports);
//			OntModel theModel = ModelFactory.createOntologyModel(spec);
//			return theModel;
//		}
//		else {
//			OntModel theModel = ModelFactory.createOntologyModel(OntModelSpec.getDefaultSpec(OWL.getURI()));
//			return theModel;
//		}
//	}

	private StringBuilder addExternalMappings(StringBuilder sb, String prjDir) throws IOException, JenaProcessorException {
		// find all .url files
		FileFilter filter = new FileFilter() {
			@Override
			public boolean accept(File pathname) {
				if (pathname.isDirectory()) {
					return true;
				}
				return pathname.isFile() && pathname.getName().endsWith(".url");
			}
		};
		List<File> urlFiles = findUrlFiles(new ArrayList<File>(), new File(prjDir), filter);
		if (urlFiles != null && urlFiles.size() > 0) {
			SadlUtils su = new SadlUtils();
			for (int i = 0; urlFiles != null && i < urlFiles.size(); i++) {
				File urlFile = urlFiles.get(i);
				String content = su.fileToString(urlFile);
				List<String>[] unp = su.getUrlsAndPrefixesFromExternalUrlContent(content);
				if (unp != null) {
					List<String> urls = unp[0];
					List<String> prefixes = unp[1];
					for (int j = 0; j < urls.size(); j++) {
						String url = urls.get(j);
						String prefix = (prefixes != null && prefixes.size() > j) ? prefixes.get(j) : null;
						String urlFilename = su.getExternalModelRootFromUrlFilename(urlFile);
						String altUrl = fileNameToFileUrl(prjDir + File.separator + urlFilename + File.separator + su.externalUrlToRelativePath(url));
						String pubUri = su.getPublicUriFromUrl(url, altUrl);
						System.out.println("Found external mapping: " + prefix  + " -> " + url + " at " + altUrl);
//						sb.replace(0, sb.length(), addMappingToPolicyFile(sb.toString(), url, altUrl, prefix, EXTERNAL_URL));
						// TODO this will break external models--must resolve alternate mapping addition using ConfigurationManagerForIDE
					}
				}
			}
		}
		return sb;
	}

	private List<File> findUrlFiles(List<File> found, File folder, FileFilter filter) {
		if (folder.isDirectory()) {
			File[] matchingContents = folder.listFiles(filter);
			for (int i = 0; i < matchingContents.length; i++) {
				File mf = matchingContents[i];
				if (mf.isDirectory()) {
					found = findUrlFiles(found, mf, filter);
				}
				else {
					found.add(mf);
				}
			}
		}
		return found;
	}

}
