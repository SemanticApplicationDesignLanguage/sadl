/*******************************************************************************
 * Copyright (c) 2000, 2010 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Red Hat, Inc - changed TarFileStructureProvider to TarLeveledStructureProvider 
 *     Barry Hathaway - modified for OWL file imports
 *******************************************************************************/
package com.ge.research.sadl.ui.imports;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.zip.ZipEntry;

import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourceAttributes;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.ContainerGenerator;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.internal.wizards.datatransfer.DataTransferMessages;
import org.eclipse.ui.internal.wizards.datatransfer.TarEntry;
import org.eclipse.ui.internal.wizards.datatransfer.TarLeveledStructureProvider;
import org.eclipse.ui.wizards.datatransfer.IImportStructureProvider;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.importer.AbortDataRowException;
import com.ge.research.sadl.jena.importer.CsvImporter;
import com.ge.research.sadl.model.SadlSerializationFormat;
import com.ge.research.sadl.owl2sadl.OwlToSadl;
import com.ge.research.sadl.processing.SadlImportProcessorProvider;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.InvalidNameException;
import com.ge.research.sadl.reasoner.QueryCancelledException;
import com.ge.research.sadl.reasoner.ReasonerNotFoundException;
import com.ge.research.sadl.reasoner.SadlJenaModelGetterPutter;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Inject;


/**
 * An operation which does the actual work of copying objects from the local file
 * system into the workspace.
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * @noextend This class is not intended to be subclassed by clients.
 */
public class CsvImportOperation extends WorkspaceModifyOperation {
	@Inject SadlImportProcessorProvider processorProvider;

    private static final int POLICY_DEFAULT = 0;

    private static final int POLICY_SKIP_CHILDREN = 1;

    private static final int POLICY_FORCE_OVERWRITE = 2;

    private Object source;

    private IPath destinationPath;

    private IContainer destinationContainer;

    private List<Object> selectedFiles;

    private List<Object> rejectedFiles;
    
    private List<?> targetFiles;

    private IImportStructureProvider provider;

    private IProgressMonitor monitor;

    protected IOverwriteQuery overwriteCallback;

    private Shell context;

    private List<IStatus> errorTable = new ArrayList<IStatus>();

    private boolean createVirtualFolder = false;

    private boolean createLinks = false;
    
    private boolean createLinkFilesOnly = false;

    private String relativeVariable = null;

    private boolean createContainerStructure = true;
    
    boolean headers = false;
    private boolean debugOutput = false;
    String namespace = "";
    List<String> imports = null; 
    String template = "";
    String destFileType = "";


    //The constants for the overwrite 3 state
    private static final int OVERWRITE_NOT_SET = 0;

    private static final int OVERWRITE_NONE = 1;

    private static final int OVERWRITE_ALL = 2;

    private int overwriteState = OVERWRITE_NOT_SET;

	private static final String ABSOLUTE_PATH = "<Absolute Path>"; //$NON-NLS-1$

	/**
     * Creates a new operation that recursively imports the entire contents of the
     * specified root file system object.
     * <p>
     * The <code>source</code> parameter represents the root file system object to 
     * import. All contents of this object are imported. Valid types for this parameter
     * are determined by the supplied <code>IImportStructureProvider</code>.
     * </p>
     * <p>
     * The <code>provider</code> parameter allows this operation to deal with the
     * source object in an abstract way. This operation calls methods on the provider
     * and the provider in turn calls specific methods on the source object.
     * </p>
     *  <p>
     * The default import behavior is to recreate the complete container structure
     * for the contents of the root file system object in their destination. 
     * If <code>setCreateContainerStructure</code> is set to false then the container 
     * structure created is relative to the root file system object.
     * </p>
     * 
     * @param containerPath the full path of the destination container within the
     *   workspace
     * @param source the root file system object to import
     * @param provider the file system structure provider to use
     * @param overwriteImplementor the overwrite strategy to use
     */
    public CsvImportOperation(IPath containerPath, Object source,
            IImportStructureProvider provider,
            IOverwriteQuery overwriteImplementor) {
        super();
        this.destinationPath = containerPath;
        this.source = source;
        this.provider = provider;
        overwriteCallback = overwriteImplementor;
    }

    /**
     * Creates a new operation that imports specific file system objects.
     * In this usage context, the specified source file system object is used by the
     * operation solely to determine the destination container structure of the file system
     * objects being imported.
     * <p>
     * The <code>source</code> parameter represents the root file system object to 
     * import. Valid types for this parameter are determined by the supplied 
     * <code>IImportStructureProvider</code>. The contents of the source which
     * are to be imported are specified in the <code>filesToImport</code>
     * parameter.
     * </p>
     * <p>
     * The <code>provider</code> parameter allows this operation to deal with the
     * source object in an abstract way. This operation calls methods on the provider
     * and the provider in turn calls specific methods on the source object.
     * </p>
     * <p>
     * The <code>filesToImport</code> parameter specifies what contents of the root
     * file system object are to be imported.
     * </p>
     * <p>
     * The default import behavior is to recreate the complete container structure
     * for the file system objects in their destination. If <code>setCreateContainerStructure</code>
     * is set to <code>false</code>, then the container structure created for each of 
     * the file system objects is relative to the supplied root file system object.
     * </p>
     *
     * @param containerPath the full path of the destination container within the
     *   workspace
     * @param source the root file system object to import from
     * @param provider the file system structure provider to use
     * @param overwriteImplementor the overwrite strategy to use
     * @param filesToImport the list of file system objects to be imported
     *  (element type: <code>Object</code>)
     */
    public CsvImportOperation(IPath containerPath, Object source,
            IImportStructureProvider provider,
            IOverwriteQuery overwriteImplementor, List<Object> filesToImport,
            boolean headers, boolean debugOutput, String namespace, List<String> imports, 
            String template, String destFileType) {
        this(containerPath, source, provider, overwriteImplementor);
        setFilesToImport(filesToImport);
        setHeaders(headers);
        setDebugOutput(debugOutput);
        setNamespace(namespace);
        setImports(imports);
        setTemplate(template);
        setDestFileType(destFileType);
    }

    /**
     * Creates a new operation that imports specific file system objects.
     * <p>
     * The <code>provider</code> parameter allows this operation to deal with the
     * source object in an abstract way. This operation calls methods on the provider
     * and the provider in turn calls specific methods on the source object.
     * </p>
     * <p>
     * The <code>filesToImport</code> parameter specifies what file system objects 
     * are to be imported.
     * </p>
     * <p>
     * The default import behavior is to recreate the complete container structure
     * for the file system objects in their destination. If <code>setCreateContainerStructure</code>
     * is set to <code>false</code>, then no container structure is created for each of 
     * the file system objects.
     * </p>
     *
     * @param containerPath the full path of the destination container within the
     *   workspace
     * @param provider the file system structure provider to use
     * @param overwriteImplementor the overwrite strategy to use
     * @param filesToImport the list of file system objects to be imported
     *  (element type: <code>Object</code>)
     */
    public CsvImportOperation(IPath containerPath,
            IImportStructureProvider provider,
            IOverwriteQuery overwriteImplementor, List<Object> filesToImport) {
        this(containerPath, null, provider, overwriteImplementor);
        setFilesToImport(filesToImport);
    }

    public boolean isHeaders() {
		return headers;
	}

	public void setHeaders(boolean headers) {
		this.headers = headers;
	}

	public String getNamespace() {
		return namespace;
	}

	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	public List<String> getImports() {
		return imports;
	}

	public void setImports(List<String> imports) {
		this.imports = imports;
	}

	public String getTemplate() {
		return template;
	}

	public void setTemplate(String template) {
		this.template = template;
	}

	public String getDestFileType() {
		return destFileType;
	}

	public void setDestFileType(String destFileType) {
		this.destFileType = destFileType;
	}

	/**
     * Prompts if existing resources should be overwritten. Recursively collects
     * existing read-only files to overwrite and resources that should not be
     * overwritten.
     * 
     * @param sourceStart destination path to check for existing files
     * @param sources file system objects that may exist in the destination
     * @param noOverwrite files that were selected to be skipped (don't overwrite).
     * 	object type IPath
     * @param overwriteReadonly the collected existing read-only files to overwrite.
     * 	object type IPath
     * @param policy on of the POLICY constants defined in the
     * class.
     */
    void collectExistingReadonlyFiles(IPath sourceStart, List<?> sources,
            ArrayList<Object> noOverwrite, ArrayList<IFile> overwriteReadonly, int policy) {
        IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
        Iterator<?> sourceIter = sources.iterator();
        IPath sourceRootPath = null;

        if (this.source != null) {
            sourceRootPath = new Path(provider.getFullPath(this.source));
        }
        while (sourceIter.hasNext()) {
            Object nextSource = sourceIter.next();
            IPath sourcePath = new Path(provider.getFullPath(nextSource));
            IPath newDestinationPath;
            IResource newDestination;

            if (sourceRootPath == null) {
                newDestinationPath = sourceStart.append(provider
                        .getLabel(nextSource));
            } else {
                int prefixLength = sourcePath
                        .matchingFirstSegments(sourceRootPath);
                IPath relativeSourcePath = sourcePath
                        .removeFirstSegments(prefixLength);
                newDestinationPath = this.destinationPath
                        .append(relativeSourcePath);
            }
            newDestination = workspaceRoot.findMember(newDestinationPath);
            if (newDestination == null) {
				continue;
			}

            IFolder folder = getFolder(newDestination);
            if (folder != null) {
                if (policy != POLICY_FORCE_OVERWRITE) {
                    if (this.overwriteState == OVERWRITE_NONE
                            || !queryOverwrite(newDestinationPath)) {
                        noOverwrite.add(folder);
                        continue;
                    }
                }
                if (provider.isFolder(nextSource)) {
					collectExistingReadonlyFiles(newDestinationPath, provider
                            .getChildren(nextSource), noOverwrite,
                            overwriteReadonly, POLICY_FORCE_OVERWRITE);
				}
            } else {
                IFile file = getFile(newDestination);

                if (file != null) {
                    if (!queryOverwriteFile(file, policy)) {
						noOverwrite.add(file.getFullPath());
					} else if (file.isReadOnly()) {
						overwriteReadonly.add(file);
					}
                }
            }
        }
    }

    /**
     * Creates the folders that appear in the specified resource path.
     * These folders are created relative to the destination container.
     *
     * @param path the relative path of the resource
     * @return the container resource coresponding to the given path
     * @exception CoreException if this method failed
     */
    IContainer createContainersFor(IPath path) throws CoreException {

        IContainer currentFolder = destinationContainer;

        int segmentCount = path.segmentCount();

        //No containers to create
        if (segmentCount == 0) {
			return currentFolder;
		}

        //Needs to be handles differently at the root
        if (currentFolder.getType() == IResource.ROOT) {
			return createFromRoot(path);
		}

        for (int i = 0; i < segmentCount; i++) {
            currentFolder = currentFolder.getFolder(new Path(path.segment(i)));
            if (!currentFolder.exists()) {
                if (createVirtualFolder)
					((IFolder) currentFolder).create(IResource.VIRTUAL, true,
							null);
                else if (createLinks)
                    ((IFolder) currentFolder).createLink(createRelativePath(
                            path, currentFolder), 0, null);
                else
                    ((IFolder) currentFolder).create(false, true, null);
            }
        }

        return currentFolder;
    }

    /**
     * Creates the folders that appear in the specified resource path
     * assuming that the destinationContainer begins at the root. Do not create projects.
     *
     * @param path the relative path of the resource
     * @return the container resource coresponding to the given path
     * @exception CoreException if this method failed
     */
    private IContainer createFromRoot(IPath path) throws CoreException {

        int segmentCount = path.segmentCount();

        //Assume the project exists 
        IContainer currentFolder = ((IWorkspaceRoot) destinationContainer)
                .getProject(path.segment(0));

        for (int i = 1; i < segmentCount; i++) {
            currentFolder = currentFolder.getFolder(new Path(path.segment(i)));
            if (!currentFolder.exists()) {
				((IFolder) currentFolder).create(false, true, null);
			}
        }

        return currentFolder;
    }

    /**
     * Deletes the given resource. If the resource fails to be deleted, adds a
     * status object to the list to be returned by <code>getResult</code>.
     *
     * @param resource the resource
     */
    void deleteResource(IResource resource) {
        try {
            resource.delete(IResource.KEEP_HISTORY, null);
        } catch (CoreException e) {
            errorTable.add(e.getStatus());
        }
    }

    /* (non-Javadoc)
     * Method declared on WorkbenchModifyOperation.
     * Imports the specified file system objects from the file system.
     */
    protected void execute(IProgressMonitor progressMonitor) {

        monitor = progressMonitor;

        try {
            if (selectedFiles == null) {
                //Set the amount to 1000 as we have no idea of how long this will take
                monitor.beginTask(DataTransferMessages.DataTransfer_importTask, 1000);
                ContainerGenerator generator = new ContainerGenerator(
                        destinationPath);
                monitor.worked(30);
                targetFiles = generateTargets(Arrays.asList(new Object[] { source }));
                validateFiles(targetFiles);
                monitor.worked(50);
                destinationContainer = generator
                        .generateContainer(new SubProgressMonitor(monitor, 50));
                importRecursivelyFrom(source, targetFiles, POLICY_DEFAULT);
                //Be sure it finishes
                monitor.worked(90);
            } else {
                // Choose twice the selected files size to take folders into account
                int creationCount = selectedFiles.size();
                monitor.beginTask(DataTransferMessages.DataTransfer_importTask, creationCount + 100);
                ContainerGenerator generator = new ContainerGenerator(
                        destinationPath);
                monitor.worked(30);
                targetFiles = generateTargets(selectedFiles);
                validateFiles(targetFiles);
                monitor.worked(50);
                destinationContainer = generator
                        .generateContainer(new SubProgressMonitor(monitor, 50));
                importFileSystemObjects(selectedFiles, targetFiles);
                monitor.done();
            }
        } catch (CoreException e) {
            errorTable.add(e.getStatus());
        } catch (Throwable t) {
        	System.err.println(t.getMessage());
        } finally {
            monitor.done();
        }
    }

    private List<File> generateTargets(List<Object> filesToImport) {
    	List<File> targets = new ArrayList<File>();
        Iterator<Object> filesEnum = filesToImport.iterator();
        while (filesEnum.hasNext()) {
            Object fileObject = filesEnum.next();
            File targetFile = getTargetFromInput(fileObject);
            targets.add(targetFile);
        }
		return targets;
	}
    
    private File getTargetFromInput(Object inputFileObject) {
        String inputFN = ((File)inputFileObject).getName();
       return new File(inputFN.substring(0, inputFN.lastIndexOf(".")) + destFileType);
    }

	/**
     * Returns the container resource that the passed file system object should be
     * imported into.
     *
     * @param fileSystemObject the file system object being imported
     * @return the container resource that the passed file system object should be
     *     imported into
     * @exception CoreException if this method failed
     */
    IContainer getDestinationContainerFor(Object fileSystemObject)
            throws CoreException {
        IPath pathname = new Path(provider.getFullPath(fileSystemObject));

        if (createContainerStructure) {
			return createContainersFor(pathname.removeLastSegments(1));
		}
        if (source == fileSystemObject) {
			return null;
		}
        IPath sourcePath = new Path(provider.getFullPath(source));
        IPath destContainerPath = pathname.removeLastSegments(1);
        IPath relativePath = destContainerPath.removeFirstSegments(
                sourcePath.segmentCount()).setDevice(null);
        return createContainersFor(relativePath);
        
    }

    /**
     * Returns the resource either casted to or adapted to an IFile. 
     * 
     * @param resource resource to cast/adapt
     * @return the resource either casted to or adapted to an IFile.
     * 	<code>null</code> if the resource does not adapt to IFile 
     */
    IFile getFile(IResource resource) {
        if (resource instanceof IFile) {
            return (IFile) resource;
        }
        Object adapted = ((IAdaptable) resource).getAdapter(IFile.class);
        if(adapted == null) {
			return null;
		}
        return (IFile) adapted;
      
    }

    /**
     * Returns the resource either casted to or adapted to an IFolder. 
     * 
     * @param resource resource to cast/adapt
     * @return the resource either casted to or adapted to an IFolder.
     * 	<code>null</code> if the resource does not adapt to IFolder 
     */
    IFolder getFolder(IResource resource) {
        if (resource instanceof IFolder) {
            return (IFolder) resource;
        }
        Object adapted = ((IAdaptable) resource).getAdapter(IFolder.class);
        if(adapted == null) {
			return null;
		}
        return (IFolder) adapted;
    }

    /**
     * Returns the rejected files based on the given multi status.
     *  
     * @param multiStatus multi status to use to determine file rejection
     * @param files source files
     * @return list of rejected files as absolute paths. Object type IPath.
     */
    ArrayList<Object> getRejectedFiles(IStatus multiStatus, IFile[] files) {
        ArrayList<Object> filteredFiles = new ArrayList<Object>();

        IStatus[] status = multiStatus.getChildren();
        for (int i = 0; i < status.length; i++) {
            if (status[i].isOK() == false) {
            	errorTable.add(status[i]);
            	filteredFiles.add(files[i].getFullPath());
            }
        }
        return filteredFiles;
    }

    /**
     * Returns the status of the import operation.
     * If there were any errors, the result is a status object containing
     * individual status objects for each error.
     * If there were no errors, the result is a status object with error code <code>OK</code>.
     *
     * @return the status
     */
    public IStatus getStatus() {
        IStatus[] errors = new IStatus[errorTable.size()];
        errorTable.toArray(errors);
        return new MultiStatus(PlatformUI.PLUGIN_ID, IStatus.OK, errors,
                DataTransferMessages.ImportOperation_importProblems,
                null);
    }

    /**
     * Imports the specified file system object into the workspace.
     * If the import fails, adds a status object to the list to be returned by
     * <code>getResult</code>.
     *
     * @param fileObject the file system object to be imported
     * @param policy determines how the file object is imported
     */
    void importFile(Object fileObject, Object fileOutObject, int policy) {
        IContainer containerResource;
        try {
            containerResource = getDestinationContainerFor(fileObject);
        } catch (CoreException e) {
            IStatus coreStatus = e.getStatus();
            String newMessage = NLS.bind(DataTransferMessages.ImportOperation_coreImportError, fileObject, coreStatus.getMessage());
            IStatus status = new Status(coreStatus.getSeverity(), coreStatus
                    .getPlugin(), coreStatus.getCode(), newMessage, null);
            errorTable.add(status);
            return;
        }

        String fileObjectPath = provider.getFullPath(fileObject);
        monitor.subTask(fileObjectPath);
        File targetFile = (File)fileOutObject;
        IFile targetResource = containerResource.getFile(new Path(provider
                .getLabel(targetFile)));
        monitor.worked(1);

        if (rejectedFiles.contains(targetResource.getFullPath())) {
			return;
		}

        // ensure that the source and target are not the same
        IPath targetPath = targetResource.getLocation();
        // Use Files for comparison to avoid platform specific case issues
        if (targetPath != null
                && (targetPath.toFile().equals(new File(fileObjectPath)))) {
            errorTable.add(new Status(IStatus.ERROR, PlatformUI.PLUGIN_ID, 0,
                    NLS.bind(DataTransferMessages.ImportOperation_targetSameAsSourceError, fileObjectPath), null));
            return;
        }

        InputStream contentStream = null;
        CsvImporter csvImporter = null;
        String kbformat = null;	// the format of OWL files in the project
        String finalFormat = null;	// the format used by the CsvImporter
        SadlJenaModelGetterPutter modelGetter = null;
        String jenaTDBFolder = null;
        String owlOutputFile = null;
        try {
        	java.net.URI projectUri = null; 
        	IContainer cont = containerResource;
       		do {
       			if (cont instanceof IProject) {
       				projectUri = ((IProject)cont).getLocationURI();
       				//org.eclipse.jface.resource.ResourceManager.getProjectUri(URI.createURI(targetPath.toPortableString()));
       				break;
       			}
       			else if (cont instanceof IFolder){
         			cont = ((IFolder)cont).getParent();
        		}
        	} while (cont != null);
        	String modelFolder = projectUri.getRawPath() + File.separator + ResourceManager.OWLDIR;
        	IConfigurationManager cmgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolder, ConfigurationManagerForIDE.getOWLFormat());;
			SadlUtils su = new SadlUtils();
        	if (cmgr == null) {
        		String modfldr = su.fileUrlToFileName(modelFolder);
        		owlOutputFile = targetPath.toPortableString();
        		cmgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolder, ConfigurationManagerForIDE.getOWLFormat());  
    	    	modelGetter = new SadlJenaModelGetterPutter(cmgr, cmgr.getTdbFolder(), kbformat);
    	    	cmgr.setModelGetter(modelGetter);
        	}
        	csvImporter = new CsvImporter(cmgr);
        	if (debugOutput) {
        		String tempFolder = projectUri.toString() + File.separator + "Temp";
        		File tfFile = new File(tempFolder);
        		tfFile.mkdirs();
        		String tripleFile = tempFolder + File.separator + targetResource.getName() + ".tripleCreationLog";
        		csvImporter.enableTriplesAddedInOrderLogging(su.fileUrlToFileName(tripleFile));
        	}
        	csvImporter.setModelFolder(modelFolder);
        	csvImporter.setImportFilename(fileObjectPath, headers);
        	csvImporter.setImportModelNamespace(namespace);
        	csvImporter.setImports(imports);
        	csvImporter.setTemplates(template); //(templateString);
        	finalFormat = csvImporter.getOwlModelFormat();
        	if (finalFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
        		jenaTDBFolder = csvImporter.getSaveAsFileName();
        		File newTargetFile = new File(jenaTDBFolder);
        		targetResource = containerResource.getFile(new Path(provider
                        .getLabel(newTargetFile)));
                targetPath = targetResource.getLocation();
                // Use Files for comparison to avoid platform specific case issues
                if (targetPath != null
                        && (targetPath.toFile().equals(new File(fileObjectPath)))) {
                    errorTable.add(new Status(IStatus.ERROR, PlatformUI.PLUGIN_ID, 0,
                            NLS.bind(DataTransferMessages.ImportOperation_targetSameAsSourceError, fileObjectPath), null));
                    return;
                }
        	}
        	else {
	    		if (this.destFileType.equalsIgnoreCase(".sadl")) {
					OwlToSadl ots;
					try {
						ots = new OwlToSadl(csvImporter.getOwlModel(), csvImporter.getModelName());
						if (isDebugOutput()) {
							ots.setVerboseMode(true);
						}
						String sadlmodel = ots.getSadlModel();
						// convert string to InputStream
						contentStream = new ByteArrayInputStream(sadlmodel.getBytes());
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
	        	}
	    		else {
	    			contentStream = csvImporter.getOwlModel(null).getInputStream();
	    		}
        	} 
		} catch (Exception e1) {
			e1.printStackTrace();
	        if (contentStream == null) {
	            errorTable
	                    .add(new Status(
	                            IStatus.ERROR,
	                            PlatformUI.PLUGIN_ID,
	                            0,
	                            e1.toString(),
	                            null));
	            return;
	        }
		}

        try {
            if (contentStream != null) {
            	String fn = csvImporter.getSaveAsFileName();
            	String prjpath = containerResource.getLocation().toString();
            	String trfn = targetResource.getFullPath().toPortableString();
            	if (fn != null && fn.indexOf(prjpath) >= 0) {
            		String relfn = fn.substring(fn.indexOf(prjpath) + prjpath.length());
            		fn = relfn;
            	}
            	if (fn != null && !targetResource.getLocation().toString().equals(fn)) {
            		targetResource = containerResource.getFile(new Path(fn)); 
            	}
	            if (targetResource.exists()) {
					targetResource.setContents(contentStream,
	                        IResource.KEEP_HISTORY, null);
	            } else {
	                if (createVirtualFolder || createLinks || createLinkFilesOnly)
	                    targetResource.createLink(createRelativePath(
	                            new Path(provider
	                                    .getFullPath(fileObject)), targetResource), 0, null);
	                else
	                    targetResource.create(contentStream, false, null);
	            }
	            setResourceAttributes(targetResource, fileObject);
	            if (!this.destFileType.equalsIgnoreCase(".sadl")) {
	            	// don't save mapping for SADL file (will be converted to OWL and mapped by resource processor) 
	            	csvImporter.saveSadlProjectOwlFileMapping(namespace, targetResource.getRawLocation().toPortableString(), null);
	            }
	
	            if (provider instanceof TarLeveledStructureProvider) {
	            	try {
	            		targetResource.setResourceAttributes(((TarLeveledStructureProvider) provider).getResourceAttributes(fileObject));
	            	} catch (CoreException e) {
	            		errorTable.add(e.getStatus());
	            	}
	            }
            }
            else {
               	long numTriples = csvImporter.processImport();
               	if (numTriples > 0) {
               		if (kbformat != null && !kbformat.equals(finalFormat)) {
                    	if (finalFormat.equals(SadlSerializationFormat.JENA_TDB_FORMAT)) {
                    		File fout = new File(owlOutputFile);
                    		Dataset ds = TDBFactory.createDataset(jenaTDBFolder);
                    		Model mod = ds.getDefaultModel();
                    		FileWriter fw;
                    		try {
                    			fw = new FileWriter(fout);
                    			BufferedWriter bw = new BufferedWriter(fw);
                    			mod.write(bw, "RDF/XML");
                    			bw.close();
                    			fw.close();
                     			TDB.sync(ds);
                    			ds.close();
                    			TDB.closedown();
                        		File tdb = new File(targetPath.toPortableString());
                        		if (tdb.exists()) {
                        			tdb.delete();
                        		}
                          	} catch (IOException e) {
                    			// TODO Auto-generated catch block
                    			e.printStackTrace();
                    		}
                    	}
               		}
               	}
        		
               	if (owlOutputFile != null) {
               		csvImporter.saveSadlProjectOwlFileMapping(namespace, owlOutputFile, null);
               	}
            }
        } catch (CoreException e) {
            errorTable.add(e.getStatus());
        } catch (ConfigurationException e) {
        	errorTable.add(new Status(Status.ERROR, SadlActivator.COM_GE_RESEARCH_SADL_SADL, e.getMessage(), e));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidNameException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (AbortDataRowException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (QueryCancelledException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ReasonerNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} finally {
            try {
            	if (contentStream != null) {
            		contentStream.close();
            	}
            } catch (IOException e) {
                errorTable
                        .add(new Status(
                                IStatus.ERROR,
                                PlatformUI.PLUGIN_ID,
                                0,
                                NLS.bind(DataTransferMessages.ImportOperation_closeStreamError, fileObjectPath),
                                e));
            }
        }
    }

    /**
     * Reuse the file attributes set in the import.
     * @param targetResource
     * @param fileObject
     */
    private void setResourceAttributes(IFile targetResource, Object fileObject) {
    	
    	long timeStamp = 0;
    	if(fileObject instanceof File) {
			try {
				targetResource.setResourceAttributes(ResourceAttributes.fromFile((File) fileObject));
				timeStamp = ((File)fileObject).lastModified();
			} catch (CoreException e) {
        		errorTable.add(e.getStatus());
			}
		}else if (fileObject instanceof TarEntry) {
        	try {
        		targetResource.setResourceAttributes(((TarLeveledStructureProvider) provider).getResourceAttributes(fileObject));
        		timeStamp = ((TarEntry)fileObject).getTime()*1000; // TarEntry time is in secs. Convert to msecs
        	} catch (CoreException e) {
        		errorTable.add(e.getStatus());
        	}
        }else if (fileObject instanceof ZipEntry) {
			long zipTimeStamp = ((ZipEntry)fileObject).getTime();
			if(zipTimeStamp != -1)
				timeStamp = zipTimeStamp;
        }
		
    	if(timeStamp!= 0) {
    		try {
				targetResource.setLocalTimeStamp(timeStamp);
			} catch (CoreException e) {
        		errorTable.add(e.getStatus());
			}
    	}
	}

	/**
     * Imports the specified file system objects into the workspace.
     * If the import fails, adds a status object to the list to be returned by
     * <code>getStatus</code>.
     *
     * @param filesToImport the list of file system objects to import
     *   (element type: <code>Object</code>)
	 * @throws CoreException 
     * @exception OperationCanceledException if canceled
     */
    void importFileSystemObjects(List<Object> filesToImport, List<?> targetFiles) throws CoreException {
        Iterator<Object> filesEnum = filesToImport.iterator();
        Iterator<?> filesOutEnum = targetFiles.iterator();
        while (filesEnum.hasNext()) {
            Object fileSystemObject = filesEnum.next();
            Object fileSystemOutObject = filesOutEnum.next();
            if (source == null) {
                // We just import what we are given into the destination
                IPath sourcePath = new Path(provider
                        .getFullPath(fileSystemObject)).removeLastSegments(1);
                if (provider.isFolder(fileSystemObject) && sourcePath.isEmpty()) {
                    // If we don't have a parent then we have selected the
                    // file systems root. Roots can't copied (at least not
                    // under windows).
                    errorTable.add(new Status(IStatus.INFO,
                            PlatformUI.PLUGIN_ID, 0, DataTransferMessages.ImportOperation_cannotCopy,
                            null));
                    continue;
                }
                source = sourcePath.toFile();
            }
            importRecursivelyFrom(fileSystemObject, fileSystemOutObject, POLICY_DEFAULT);
        }
    }

    /**
     * Imports the specified file system container object into the workspace.
     * If the import fails, adds a status object to the list to be returned by
     * <code>getResult</code>.
     *
     * @param folderObject the file system container object to be imported
     * @param policy determines how the folder object and children are imported
     * @return the policy to use to import the folder's children
     * @throws CoreException 
     */
    int importFolder(Object folderObject, int policy) throws CoreException {
        IContainer containerResource;
        try {
            containerResource = getDestinationContainerFor(folderObject);
        } catch (CoreException e) {
            errorTable.add(e.getStatus());
            return policy;
        }

        if (containerResource == null) {
			return policy;
		}

        monitor.subTask(provider.getFullPath(folderObject));
        IWorkspace workspace = destinationContainer.getWorkspace();
        IPath containerPath = containerResource.getFullPath();
        IPath resourcePath = containerPath.append(provider
                .getLabel(folderObject));

        // Do not attempt the import if the resource path is unchanged. This may happen
        // when importing from a zip file.
        if (resourcePath.equals(containerPath)) {
			return policy;
		}

        if (workspace.getRoot().exists(resourcePath)) {
            if (rejectedFiles.contains(resourcePath)) {
				return POLICY_SKIP_CHILDREN;
			}

            IFolder folder = workspace.getRoot().getFolder(resourcePath);
            if (createVirtualFolder || createLinks || folder.isVirtual() || folder.isLinked()) {
                folder.delete(true, null);
            } else
                return POLICY_FORCE_OVERWRITE;
        }

        try {
            if (createVirtualFolder)
				workspace.getRoot().getFolder(resourcePath).create(
						IResource.VIRTUAL, true, null);
            else if (createLinks) {
            	IFolder newFolder = workspace.getRoot().getFolder(resourcePath);
            	newFolder.createLink(
                        createRelativePath(new Path(provider.getFullPath(folderObject)), newFolder),
                        0, null);
                policy = POLICY_SKIP_CHILDREN;
            } else
                workspace.getRoot().getFolder(resourcePath).create(false, true, null);
        } catch (CoreException e) {
            errorTable.add(e.getStatus());
        }

        return policy;
    }

    /**
     * Transform an absolute path URI to a relative path one (i.e. from
     * "C:\foo\bar\file.txt" to "VAR\file.txt" granted that the relativeVariable
     * is "VAR" and points to "C:\foo\bar\").
     * 
     * @param location
     * @param resource 
     * @return an URI that was made relative to a variable
     */
    private IPath createRelativePath(IPath location, IResource resource) {
		if (relativeVariable == null)
			return location;
		if (relativeVariable.equals(ABSOLUTE_PATH))
			return location;
		IPathVariableManager pathVariableManager = resource.getPathVariableManager();
//		try {
//			return URIUtil.toPath(pathVariableManager.convertToRelative(URIUtil.toURI(location), true, relativeVariable));
//		} catch (CoreException e) {
			return location;
//		}
	}

	/**
     * Imports the specified file system object recursively into the workspace.
     * If the import fails, adds a status object to the list to be returned by
     * <code>getStatus</code>.
     *
     * @param fileSystemObject the file system object to be imported
     * @param policy determines how the file system object and children are imported
	 * @throws CoreException 
     * @exception OperationCanceledException if canceled
     */
    void importRecursivelyFrom(Object fileSystemObject, Object fileSystemOutObject, int policy) throws CoreException {
        if (monitor.isCanceled()) {
			throw new OperationCanceledException();
		}

        if (!provider.isFolder(fileSystemObject)) {
            importFile(fileSystemObject, fileSystemOutObject, policy);
            return;
        }

        int childPolicy = importFolder(fileSystemObject, policy);
        if (childPolicy != POLICY_SKIP_CHILDREN) {
            Iterator<?> children = provider.getChildren(fileSystemObject)
                    .iterator();
            while (children.hasNext()) {
            	Object fso = children.next();
				importRecursivelyFrom(fso, getTargetFromInput(fso), childPolicy);
			}
        }
    }

    /**
     * Queries the user whether the resource with the specified path should be
     * overwritten by a file system object that is being imported.
     * 
     * @param resourcePath the workspace path of the resource that needs to be overwritten
     * @return <code>true</code> to overwrite, <code>false</code> to not overwrite
     * @exception OperationCanceledException if canceled
     */
    boolean queryOverwrite(IPath resourcePath)
            throws OperationCanceledException {
        String overwriteAnswer = overwriteCallback.queryOverwrite(resourcePath
                .makeRelative().toString());

        if (overwriteAnswer.equals(IOverwriteQuery.CANCEL)) {
			throw new OperationCanceledException(DataTransferMessages.DataTransfer_emptyString);
		}

        if (overwriteAnswer.equals(IOverwriteQuery.NO)) {
            return false;
        }

        if (overwriteAnswer.equals(IOverwriteQuery.NO_ALL)) {
            this.overwriteState = OVERWRITE_NONE;
            return false;
        }

        if (overwriteAnswer.equals(IOverwriteQuery.ALL)) {
			this.overwriteState = OVERWRITE_ALL;
		}

        return true;
    }

    /**
     * Returns whether the given file should be overwritten.
     *
     * @param targetFile the file to ask to overwrite 
     * @param policy determines if the user is queried for overwrite 
     * @return <code>true</code> if the file should be overwritten, and
     * 	<code>false</code> if not.
     */
    boolean queryOverwriteFile(IFile targetFile, int policy) {
        //If force overwrite is on don't bother
        if (policy != POLICY_FORCE_OVERWRITE) {
            if (this.overwriteState == OVERWRITE_NOT_SET
                    && !queryOverwrite(targetFile.getFullPath())) {
				return false;
			}
            if (this.overwriteState == OVERWRITE_NONE) {
				return false;
			}
        }
        return true;
    }

    /**
     * Sets the context for use by the VCM provider to prompt the user
     * for check-out of files.
     * 
     * @param shell context for use by the VCM provider to prompt user
     * 	for check-out. The user will not be prompted if set to <code>null</code>.
     * @see IWorkspace#validateEdit(org.eclipse.core.resources.IFile[], java.lang.Object)
     * @since 2.1
     */
    public void setContext(Shell shell) {
        context = shell;
    }

    /**
     * Sets whether the containment structures that are implied from the full paths
     * of file system objects being imported should be duplicated in the workbench.
     *
     * @param value <code>true</code> if containers should be created, and
     *  <code>false</code> otherwise
     */
    public void setCreateContainerStructure(boolean value) {
        createContainerStructure = value;
    }

    /**
     * Sets the file system objects to import.
     *
     * @param filesToImport the list of file system objects to be imported
     *   (element type: <code>Object</code>)
     */
    public void setFilesToImport(List<Object> filesToImport) {
        this.selectedFiles = filesToImport;
    }

    /**
     * Sets whether imported file system objects should automatically overwrite
     * existing workbench resources when a conflict occurs.
     *
     * @param value <code>true</code> to automatically overwrite, and 
     *   <code>false</code> otherwise
     */
    public void setOverwriteResources(boolean value) {
        if (value) {
			this.overwriteState = OVERWRITE_ALL;
		}
    }

    /**
     * Validates that the given source resources can be copied to the 
     * destination as decided by the VCM provider.
     * 
     * @param existingFiles existing files to validate
     * @return list of rejected files as absolute paths. Object type IPath.
     */
    ArrayList<Object> validateEdit(List<IFile> existingFiles) {
       
        if (existingFiles.size() > 0) {
            IFile[] files = (IFile[]) existingFiles
                    .toArray(new IFile[existingFiles.size()]);
            IWorkspace workspace = ResourcesPlugin.getWorkspace();
            IStatus status = workspace.validateEdit(files, context);

            //If there was a mix return the bad ones
            if (status.isMultiStatus()) {
				return getRejectedFiles(status, files);
			}
            
           if(!status.isOK()){
           		//If just a single status reject them all
           		errorTable.add(status);
           		ArrayList<Object> filteredFiles = new ArrayList<Object>();

           		for (int i = 0; i < files.length; i++) {
           			filteredFiles.add(files[i].getFullPath());
           		}
           		return filteredFiles;
           }
            
        }
        return new ArrayList<Object>();
    }

    /**
     * Validates the given file system objects.
     * The user is prompted to overwrite existing files.
     * Existing read-only files are validated with the VCM provider.
     * 
     * @param sourceFiles files to validate
     */
    void validateFiles(List<?> sourceFiles) {
        ArrayList<Object> noOverwrite = new ArrayList<Object>();
        ArrayList<IFile> overwriteReadonly = new ArrayList<IFile>();

        collectExistingReadonlyFiles(destinationPath, sourceFiles, noOverwrite,
                overwriteReadonly, POLICY_DEFAULT);
        rejectedFiles = validateEdit(overwriteReadonly);
        rejectedFiles.addAll(noOverwrite);
    }

    /**
     * Set Whether groups and links will be created instead of files and folders
     * 
     * @param virtualFolders
     * @since 3.6
     */
    public void setVirtualFolders(boolean virtualFolders) {
        createVirtualFolder = virtualFolders;
    }

    /**
     * Set Whether links will be created instead of files and folders
     * 
     * @param links
     * @since 3.6
     */
    public void setCreateLinks(boolean links) {
        createLinks = links;
    }

    /**
     * Set a variable relative to which the links are created
     * 
     * @param variable
     * @since 3.6
     */
    public void setRelativeVariable(String variable) {
        relativeVariable = variable;
    }

	boolean isDebugOutput() {
		return debugOutput;
	}

	void setDebugOutput(boolean debugOutput) {
		this.debugOutput = debugOutput;
	}
}
