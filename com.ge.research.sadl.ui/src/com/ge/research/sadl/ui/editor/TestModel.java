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

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.xtext.resource.XtextResourceSet;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.ConfigurationType;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Inject;
import com.google.inject.Injector;

public class TestModel extends SadlActionDelegate implements IObjectActionDelegate {
	private static final String pSHOWNAMESPACES = "pShowNamespaces";
	private static final String[] CONSOLE = {"Console"};

    @Inject
    private XtextResourceSet resourceSet;
    
    @Inject
    private SadlModelManager visitor;

	public TestModel () {
		Injector injector = SadlActivator.getInstance().getInjector("com.ge.research.sadl.Sadl");//new SadlStandaloneSetup().createInjectorAndDoEMFRegistration();
		injector.injectMembers(this);
	}

	@Override
	protected void run(IPath testFilePath) {
    	IPreferencesService service = Platform.getPreferencesService();
		final boolean validateBeforeTesting = service.getBoolean("com.ge.research.sadl.Sadl", "validateBeforeTest", false, null);
		final boolean showReasonerTimingInformation = service.getBoolean("com.ge.research.sadl.Sadl", "showTimingInformation", false, null);
	    boolean showNS = service.getBoolean("com.ge.research.sadl.Sadl", "namespacesInQueryResults", false, null); 

	    long t1 = 0L;
	    if (showReasonerTimingInformation) {
	    	t1 = System.currentTimeMillis();
	    }
	    prepareModel(visitor, testFilePath, resourceSet);
		final String modelName = visitor.getModelName();
	    
		ConfigurationItem ci = new ConfigurationItem(CONSOLE);
		NameValuePair nvp = new NameValuePair(pSHOWNAMESPACES, showNS);
		nvp.setConfigType(ConfigurationType.SingleValue);
		ci.addNameValuePair(nvp);
		try {
			visitor.updateConfiguration(ci);
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		
		if (showReasonerTimingInformation) {
			long t2 = System.currentTimeMillis();
			SadlConsole.writeToConsole(MessageType.INFO, "Time (ms) to prepare model to run in IDE: " + (t2 - t1) + "\n\n");
		}

	    SadlConsole.writeToConsole(MessageType.INFO, "Running tests: " + testFilePath.lastSegment() + "(" + modelName + ")\n");
	    
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
				visitor.runAllTests(modelName, validateBeforeTesting, showReasonerTimingInformation);
				SadlConsole.displayMessages(visitor);
				return Status.OK_STATUS;
  			}
  			
  		};
		runTestJob.schedule();
		

	}

}
