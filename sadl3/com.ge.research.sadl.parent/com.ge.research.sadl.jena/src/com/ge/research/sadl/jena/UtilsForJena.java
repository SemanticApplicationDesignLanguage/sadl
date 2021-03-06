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

import com.ge.research.sadl.model.gp.NamedNode;
import com.ge.research.sadl.model.gp.NamedNode.NodeType;
import com.ge.research.sadl.processing.SadlConstants;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.utils.ResourceManager;
import org.apache.jena.ontology.CardinalityRestriction;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.IntersectionClass;
import org.apache.jena.ontology.MaxCardinalityRestriction;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.ontology.OntProperty;
import org.apache.jena.ontology.Restriction;
import org.apache.jena.ontology.UnionClass;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.OWL2;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.XSD;

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

    protected String rdfNodeToString(RDFNode node) {
    	if (node != null) {
			if (node instanceof Literal) {
				return ((Literal)node).getValue().toString();
			}
			return node.toString();
    	}
    	return null;
	}

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
	
	/*
	 * Methods to handle SADL typed lists
	 */

	/**
	 * Method to get the list type of a typed list
	 * @param theJenaModel
	 * @param configMgr
	 * @param modelNamespace
	 * @param node
	 * @return
	 * @throws TranslationException
	 */
	public static NamedNode getTypedListType(OntModel theJenaModel, IConfigurationManager configMgr, 
			String modelNamespace, RDFNode node) throws TranslationException {
		if (node.isResource()) {
			StmtIterator sitr = theJenaModel.listStatements(node.asResource(), RDFS.subClassOf, (RDFNode) null);
			NodeType tctypetype = null;
			RDFNode type = null;
			int lMaxLengthRestriction = -1;
			int lMinLengthRestriction = -1;
			int lLengthRestriction = -1;
			while (sitr.hasNext()) {
				RDFNode supercls = sitr.nextStatement().getObject();
				if (supercls.isResource()) {
					if (supercls.asResource().hasProperty(OWL.onProperty,
							theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
						Statement avfstmt = supercls.asResource().getProperty(OWL.allValuesFrom);
						if (avfstmt != null) {
							type = avfstmt.getObject();
							if (type.isURIResource()) {
								tctypetype = NodeType.ClassListNode;
								if (type.asResource().getNameSpace().equals(XSD.getURI())) {
									tctypetype = NodeType.DataTypeListNode;
								}
								else {
									StmtIterator eqitr = type.asResource().listProperties(OWL.equivalentClass);
									if (eqitr.hasNext()) {
										RDFNode eqCls = eqitr.nextStatement().getObject();
										if (eqCls.equals(RDFS.Datatype)) {
											tctypetype = NodeType.DataTypeListNode;
										}
									}
								}
							}
						}
					}
					if(supercls.asResource().hasProperty(OWL.onProperty,
							theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_MAXLENGTH_RESTRICTION_URI))) {
						Statement lHasValueStmt = supercls.asResource().getProperty(OWL.hasValue);
						if(lHasValueStmt != null) {
							lMaxLengthRestriction = lHasValueStmt.getObject().asLiteral().getInt();
							
						}
					}
					if(supercls.asResource().hasProperty(OWL.onProperty,
							theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_MINLENGTH_RESTRICTION_URI))) {
						Statement lHasValueStmt = supercls.asResource().getProperty(OWL.hasValue);
						if(lHasValueStmt != null) {
							lMinLengthRestriction = lHasValueStmt.getObject().asLiteral().getInt();
							
						}
					}
					if(supercls.asResource().hasProperty(OWL.onProperty,
							theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_LENGTH_RESTRICTION_URI))) {
						Statement lHasValueStmt = supercls.asResource().getProperty(OWL.hasValue);
						if(lHasValueStmt != null) {
							lLengthRestriction = lHasValueStmt.getObject().asLiteral().getInt();
							
						}
					}
				}
			}
			if(tctypetype != null) {
				NamedNode tctype = validateNamedNode(configMgr, modelNamespace, new NamedNode(type.asResource().getURI(),
						tctypetype));
				tctype.setMaxListLength(lMaxLengthRestriction);
				tctype.setMinListLength(lMinLengthRestriction);
				tctype.setListLength(lLengthRestriction);
				return tctype;
			}
			
			// maybe it's an instance
			if (node.asResource().canAs(Individual.class)) {
				ExtendedIterator<org.apache.jena.rdf.model.Resource> itr = node.asResource().as(Individual.class)
						.listRDFTypes(true);
				while (itr.hasNext()) {
					org.apache.jena.rdf.model.Resource r = itr.next();
					sitr = theJenaModel.listStatements(r, RDFS.subClassOf, (RDFNode) null);
					while (sitr.hasNext()) {
						RDFNode supercls = sitr.nextStatement().getObject();
						if (supercls.isResource()) {
							if (supercls.asResource().hasProperty(OWL.onProperty,
									theJenaModel.getResource(SadlConstants.SADL_LIST_MODEL_FIRST_URI))) {
								Statement avfstmt = supercls.asResource().getProperty(OWL.allValuesFrom);
								if (avfstmt != null) {
									type = avfstmt.getObject();
									if (type.isURIResource()) {
										NamedNode tctype = validateNamedNode(configMgr, modelNamespace, new NamedNode(type.asResource().getURI(),
												NodeType.ClassNode));
										sitr.close();
										return tctype;
									}
								}
							}
							
						}
					}
				}
			}
		}
		return null;
	}
	
	/**
	 * Method to validate a NamedNode by making sure it has a namespace and prefix
	 * @param configMgr
	 * @param modelNamespace
	 * @param namedNode
	 * @return
	 */
	public static NamedNode validateNamedNode(IConfigurationManager configMgr, 
			String modelNamespace, NamedNode namedNode) {
		if (namedNode.getPrefix() == null) {
			if (namedNode.getNamespace() == null) {
				namedNode.setNamespace(modelNamespace);
			}
			if (configMgr != null) {
				namedNode.setPrefix(configMgr.getGlobalPrefix(namedNode.getNamespace()));
			}
		}
		return namedNode;
	}

	/**
	 * Method to determine if an RDFNode is a subclass of the SADL typed list
	 * @param theJenaModel
	 * @param node
	 * @return
	 */
	public static boolean isTypedListSubclass(OntModel theJenaModel, RDFNode node) {
		if (node != null && node.isResource()) {
			org.apache.jena.rdf.model.Resource lstcls = theJenaModel
					.getResource(SadlConstants.SADL_LIST_MODEL_LIST_URI);
			if (lstcls != null && node.asResource().hasProperty(RDFS.subClassOf, lstcls)) { // if model has no lists,
																							// the list model will not
																							// have been imported
				return true;
			}
		}
		return false;
	}

	/**
	 * Method to convert an RDFNode to a SADL string
	 * @param theJenaModel
	 * @param configMgr
	 * @param modelNamespace
	 * @param obj
	 * @return
	 */
	public static String nodeToString(OntModel theJenaModel, IConfigurationManager configMgr, 
			String modelNamespace, RDFNode obj) {
		StringBuilder sb = new StringBuilder();
		if (obj.isURIResource()) {
			sb.append(uriStringToString(configMgr, modelNamespace, obj.toString()));
		} else if (obj.canAs(UnionClass.class)) {
			UnionClass ucls = obj.as(UnionClass.class);
			ExtendedIterator<RDFNode> uitr = ucls.getOperands().iterator();
			sb.append("(");
			while (uitr.hasNext()) {
				if (sb.length() > 1) {
					sb.append(" or ");
				}
				sb.append(nodeToString(theJenaModel, configMgr, modelNamespace, uitr.next()));
			}
			sb.append(")");
		} else if (obj.canAs(IntersectionClass.class)) {
			IntersectionClass icls = obj.as(IntersectionClass.class);
			ExtendedIterator<RDFNode> iitr = icls.getOperands().iterator();
			sb.append("(");
			while (iitr.hasNext()) {
				if (sb.length() > 1) {
					sb.append(" and ");
				}
				sb.append(nodeToString(theJenaModel, configMgr, modelNamespace, iitr.next()));
			}
			sb.append(")");
		} else if (obj.isResource() && isTypedListSubclass(theJenaModel, obj)) {
			NamedNode cn;
			try {
				cn = getTypedListType(theJenaModel, configMgr, modelNamespace, obj);
				sb.append(cn.getName() + " List");
				String lLengthInfo = getListLengthAsString(cn);
				if(!lLengthInfo.isEmpty()) {
					sb.append(" " + lLengthInfo);
				}
			} catch (TranslationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				sb.append("<null>");
			}
		} else {
			sb.append("<blank node>");
		}
		return sb.toString();
	}

	/*
	 * Method to convert a URI string into a SADL string
	 */
	public static String uriStringToString(IConfigurationManager configMgr, String modelName, String uri) {
		int sep = uri.lastIndexOf('#');
		if (sep > 0) {
			String ns = uri.substring(0, sep);
			String ln = uri.substring(sep + 1);
			// if the concept is in the current model just return the localname
			if (ns.equals(modelName)) {
				return ln;
			}
			// get the prefix and if there is one generate qname
			String prefix = configMgr.getGlobalPrefix(ns);
			if (prefix == null) {
				if (ns.equals(SadlConstants.SADL_IMPLICIT_MODEL_URI)) {
					prefix = SadlConstants.SADL_IMPLICIT_MODEL_PREFIX;
				}
			}
			if (prefix != null && prefix.length() > 0) {
				return prefix + ":" + ln;
			}
			return ln;
		}
		return uri;
	}

	/**
	 * Method to convert a NamedNode with list properties into a SADL string
	 * @param node
	 * @return
	 */
	public static String getListLengthAsString(NamedNode node) {
		StringBuilder sb = new StringBuilder();	
		int length = node.getListLength();
		int minLength = node.getMinListLength();
		int maxLength = node.getMaxListLength();
		if(length != -1 || minLength != -1 || maxLength != -1) {		
			sb.append("length ");
			if(minLength != -1 || maxLength != -1) {
				if(minLength == -1) {
					sb.append("0");
				}else {
					sb.append(minLength);
				}
				sb.append("-");
				if(maxLength == -1) {
					sb.append("*");
				}else {
					sb.append(maxLength);
				}
			}else {
				sb.append(length);
			}
			sb.append(" ");
		}
		return sb.toString();
	}

}
