/************************************************************************
 * Copyright � 2007-2010 - General Electric Company, All Rights Reserved
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

package com.ge.research.sadl.ui.editor;


import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.URI;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.xtext.resource.XtextResourceSet;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.SadlUtils;
import com.ge.research.sadl.builder.ResourceManager;
import com.google.inject.Inject;
import com.google.inject.Injector;


public class TestSuite extends SadlActionDelegate implements IObjectActionDelegate {

    @Inject
    private XtextResourceSet resourceSet;
    
    @Inject
    private SadlModelManager visitor;
    
    private int errorCnt = 0;		// keep track of errors for reporting

	public TestSuite () {
		Injector injector = SadlActivator.getInstance().getInjector("com.ge.research.sadl.Sadl");//new SadlStandaloneSetup().createInjectorAndDoEMFRegistration();
		injector.injectMembers(this);
	}

	@Override
	protected void run(final IPath testFilePath) {
		errorCnt = 0;
    	IPreferencesService service = Platform.getPreferencesService();
		final boolean validateBeforeTesting = service.getBoolean("com.ge.research.sadl.Sadl", "validateBeforeTest", false, null);
		final boolean showReasonerTimingInformation = service.getBoolean("com.ge.research.sadl.Sadl", "showTimingInformation", false, null);

		if (visitor == null) {
			SadlConsole.writeToConsole(MessageType.ERROR, "Unable to run test suite until a SADL model file has been opened to initialize system.\n");
			return;
		}

	    IPath absolutePath = testFilePath.makeAbsolute();
		final List<String> testFiles = getSadlTestFiles(absolutePath);
  		Job runTestJob = new Job("Run Tests") {
  			
  			@Override
  			protected void canceling() {
  				try {
  					visitor.getConfigurationMgr(null).setInferenceCanceled(true);
  				} catch (MalformedURLException | ConfigurationException e) {
  					// TODO Auto-generated catch block
  					e.printStackTrace();
  				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
  			};

  			@Override
  			protected IStatus run(IProgressMonitor monitor) {
  				Iterator<String> itr = testFiles.iterator();
  				int total = 0;
  				int passed = 0;
				while (itr.hasNext()) {
					String sadlFile = itr.next();
					IPath sadlPath = testFilePath.uptoSegment(1).append(sadlFile);
					prepareModel(visitor, sadlPath, resourceSet);
					String modelName = visitor.getModelName();
				    SadlConsole.writeToConsole(MessageType.INFO, "\nRunning tests: " + testFilePath.lastSegment() + "(" + modelName + ")\n");
					int[] results = visitor.runAllTests(modelName, validateBeforeTesting, showReasonerTimingInformation);
					passed += results[0];
					total += results[1];
					SadlConsole.displayMessages(visitor);
				}
				SadlConsole.writeToConsole(MessageType.INFO, "Test Suite Totals: passed " + passed + " of " + total + " tests.\n");
				if (errorCnt > 0) {
					SadlConsole.writeToConsole(MessageType.ERROR, "Encountered " + errorCnt + (errorCnt == 1 ? " error.\n" : " errors.\n"));
				}
				SadlConsole.displayMessages(visitor);
				return Status.OK_STATUS;
  			}
  			
  		};
		runTestJob.schedule();
	}

	private List<String> getSadlTestFiles(IPath absolutePlatformPath) {
	    URI uri = URI.createPlatformResourceURI(absolutePlatformPath.toString(), true);
	    URI absoluteUri = ResourceManager.convertPlatformUriToAbsoluteUri(uri);
		String prjdir = ResourceManager.getProjectUri(uri).toFileString();
	    String absolutePath = absoluteUri.toFileString();
		List<String> files = getSadlTestFiles(prjdir, absolutePath);
		if (files.size() > 0) {
			return files;
		}
		return null;
	}
	
	public List<String> getSadlTestFiles(String prjdir, String absolutePath) {
		List<String> files = new ArrayList<String>();
        try {
            BufferedReader in = new BufferedReader(new FileReader(absolutePath));
            String line;
            
             while ((line = in.readLine()) != null) {
                line = line.trim();
                if (!line.startsWith("Test:")) continue;     // Skip lines that aren't tests
                int firstQuote = line.indexOf("\"");
                int lastQuote = line.lastIndexOf("\"");
                if (!(lastQuote > firstQuote)) {
                	SadlConsole.writeToConsole(MessageType.ERROR, "Line '" + line + "' appears to be missing quotes or have an invalid model name.\n");
                	errorCnt++;
                	continue;
                }
                String sadlFileName = line.substring(firstQuote + 1, lastQuote);
        		String absfn = null;
        		if (sadlFileName.startsWith("file:") || sadlFileName.indexOf(':') < 0) {
        			absfn = ResourceManager.findSadlFileInProject(prjdir, sadlFileName);
        		}
        		else if (sadlFileName.startsWith("http:")) {
        			try {
        				if (visitor != null) {
        					String modelfolder = ResourceManager.getOwlModelsFolder(URI.createFileURI(absolutePath));
        					absfn = visitor.getConfigurationMgr(modelfolder).getAltUrlFromPublicUri(sadlFileName);
        				}
        				else {
        					ConfigurationManager cmgr = new ConfigurationManager(ResourceManager.getOwlModelsFolder(URI.createFileURI(absolutePath)),
        							ConfigurationManagerForIDE.getOWLFormat());
        					absfn = cmgr.getAltUrlFromPublicUri(sadlFileName);
        				}
						URI actualUri = URI.createFileURI(SadlUtils.fileUrlToFileName(absfn));
						String sadlfn = actualUri.trimFileExtension().appendFileExtension(ResourceManager.SADLEXT).segment(actualUri.segmentCount() - 1);
						absfn = ResourceManager.findSadlFileInProject(prjdir, sadlfn);
					} catch (ConfigurationException e) {
	    				SadlConsole.writeToConsole(MessageType.ERROR, "Error converting test file (" + sadlFileName + ") to an actual URL: " + e.getLocalizedMessage() + ".\n");
						e.printStackTrace();
						errorCnt++;
					} catch (Throwable t) {
	    				SadlConsole.writeToConsole(MessageType.ERROR, "Error converting test file (" + sadlFileName + ") to an actual URL: " + t.getLocalizedMessage() + ".\n");
						t.printStackTrace();	
						errorCnt++;
					}
					
        		}
        		else {
    				SadlConsole.writeToConsole(MessageType.ERROR, "Test file '" + sadlFileName + "' is not of an expected type; must be a 'file:/' or an 'http:/' URI.\n");
    				errorCnt++;
    				continue;
        		}
        		if (absfn == null) {
    				SadlConsole.writeToConsole(MessageType.ERROR, "Test file '" + sadlFileName + " not found.\n");
        		}
        		else if (absfn.endsWith(ResourceManager.SADLEXTWITHPREFIX)) {
//        			System.out.println("Sadl File: '" + absfn + "'");
        			File fl = new File(absfn);
        			if (fl.exists() && !fl.isDirectory()) {
        				String relpath = fl.getAbsolutePath().substring(prjdir.length());
        				files.add(relpath);
        			}
        		}
                else if (absfn.endsWith(".test")) {
                	List<String> moreFiles = getSadlTestFiles(prjdir, absfn);
                	if (moreFiles != null && moreFiles.size() > 0) {
                		files.addAll(moreFiles);
                	}
                }
                else {
    				SadlConsole.writeToConsole(MessageType.ERROR, "Test file '" + sadlFileName + " is not of an expected type.\n");
    				errorCnt++;
                }
            }
        } catch (IOException e) {
			// TODO Auto-generated catch block 
			e.printStackTrace();
			SadlConsole.writeToConsole(MessageType.ERROR, "Unexpected error in test suite: " + e.getLocalizedMessage() + "\n");
			errorCnt++;
		}
        catch (Throwable t) {
        	t.printStackTrace();
			SadlConsole.writeToConsole(MessageType.ERROR, "Unexpected error in test suite: " + t.getLocalizedMessage() + "\n");
			errorCnt++;
        }
		return files;
	}

}
