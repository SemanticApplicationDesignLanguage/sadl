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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.3 $ Last modified on   $Date: 2014/10/16 17:11:49 $
 ***********************************************************************/

package com.ge.research.sadl.ui.editor;

import java.io.IOException;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.parser.IParser;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.StringInputStream;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.SadlModelManagerProvider;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.ConfigurationType;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.sadl.Model;
import com.ge.research.sadl.sadl.Query;
import com.ge.research.sadl.services.SadlGrammarAccess;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.google.inject.Inject;
import com.google.inject.Injector;

public class RunQuery extends SadlActionDelegate implements IObjectActionDelegate {
	private static final String pSHOWNAMESPACES = "pShowNamespaces";
	private static final String[] CONSOLE = {"Console"};
	public static final String pQUERY = "pQuery";
	public static final String[] ADHOCQUERY = {"AdHocQuery"};
	private static final String PARSE_TYPE = "Query";
    @Inject
	private IParser parser;
    @Inject
    private SadlGrammarAccess grammar;
   
    @Inject
	private SadlModelManagerProvider sadlModelManagerProvider;
    
    private SadlModelManager visitor = null;
    
    /**
     * Class to capture the user's input in the dialog
     * @author crapo
     *
     */
    class UserQueryInput {
    	private String query;
    	private boolean fqn;
    	private boolean canceled;

		public void setQuery(String query) {
			this.query = query;
		}

		public String getQuery() {
			return query;
		}

		public void setFqn(boolean fqn) {
			this.fqn = fqn;
		}

		public boolean isFqn() {
			return fqn;
		}

		public void setCanceled(boolean canceled) {
			this.canceled = canceled;
		}

		public boolean isCanceled() {
			return canceled;
		}
    	
    }

    public RunQuery () {
    	Injector injector = SadlActivator.getInstance().getInjector("com.ge.research.sadl.Sadl");
    	injector.injectMembers(this);
    }

	@Override
	protected void run(final IPath testFilePath) {
		IWorkbenchPage page =  PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart editorPart = page.getActiveEditor();
		if (editorPart.isDirty()) {
		    SadlConsole.writeToConsole(MessageType.ERROR, "Model has unsaved changes. Please save before running query.\n");
		}
		
		XtextResourceSet resourceSet = new XtextResourceSet();
		// TODO: [KTH] How should this work? There is no resource in the ResourceSet yet
		visitor = sadlModelManagerProvider.get(resourceSet.getResources().get(0).getURI());
		prepareModel(visitor, testFilePath, resourceSet);
		final String modelName = visitor.getModelName();
		
		String query = null;
		List<Object> previousQueries = null;
		try {
			List<ConfigurationItem> config =visitor.getConfiguration(ADHOCQUERY, false); 
			if (config != null && config.size() > 0) {
				previousQueries = config.get(0).getAllValuesOfName(pQUERY);
			}
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		if (previousQueries != null && previousQueries.size() >= 1) {
			final Shell shell = new Shell();
			shell.setText("Run Query");
			Display display = shell.getDisplay();
			shell.setLayout(new GridLayout(2, false));
			(new Label(shell, SWT.NULL)).setText("Enter a SADL or SPARQL query:");
			final UserQueryInput returnVal = new UserQueryInput();
			final Combo combo = new Combo(shell, SWT.DROP_DOWN | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
			combo.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			final Button fqnb = new Button (shell, SWT.CHECK);
			fqnb.setText("Show namespaces?");
			List<ConfigurationItem> cil;
			boolean showNamespaces = true;
			try {
				cil = visitor.getConfiguration(CONSOLE, false);
				if (cil != null && cil.size() > 0) {
					ConfigurationItem ci = cil.get(0);
					Object objVal = ci.getNamedValue(pSHOWNAMESPACES);
					if (objVal != null && objVal instanceof Boolean) {
						showNamespaces = ((Boolean)objVal).booleanValue();
					}
				}
			} catch (ConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (MalformedURLException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			fqnb.setSelection(showNamespaces);
			fqnb.addMouseListener(new MouseListener() {		
				@Override
				public void mouseUp(MouseEvent e) {
					
				}
				
				@Override
				public void mouseDown(MouseEvent e) {
				}
				
				@Override
				public void mouseDoubleClick(MouseEvent e) {
				}
			});
			
			Button cancelb = new Button (shell, SWT.PUSH);
			cancelb.setText("Cancel");
			cancelb.addMouseListener(new MouseListener() {		
				@Override
				public void mouseUp(MouseEvent e) {
				}
				
				@Override
				public void mouseDown(MouseEvent e) {
					combo.clearSelection();
//					System.out.println("On Cancel text is: " + combo.getText());
					returnVal.setQuery(null);
					returnVal.setCanceled(true);
					shell.dispose();
				}
				
				@Override
				public void mouseDoubleClick(MouseEvent e) {
				}
			});

			Button okb = new Button (shell, SWT.PUSH);
			okb.setText("OK");
			okb.addMouseListener(new MouseListener() {		
				@Override
				public void mouseUp(MouseEvent e) {
				}
				
				@Override
				public void mouseDown(MouseEvent e) {
//					System.out.println("On OK combo text is: " + combo.getText());
					returnVal.setFqn(fqnb.getSelection());
					returnVal.setQuery(combo.getText());
					shell.dispose();
				}
				
				@Override
				public void mouseDoubleClick(MouseEvent e) {
				}
			});

			combo.addSelectionListener(new org.eclipse.swt.events.SelectionListener() {

				@Override
				public void widgetSelected(SelectionEvent e) {
					// TODO Auto-generated method stub
//					System.out.println("Selected: " + combo.getSelectionIndex() + ", " + combo.getItem(combo.getSelectionIndex()));
//					System.out.println("  check: " + combo.getText());
					returnVal.setQuery(combo.getItem(combo.getSelectionIndex()));
					returnVal.setQuery(combo.getText());
					}
					
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
					String text = combo.getText();
					if (combo.indexOf(text) == -1) {
						combo.add(text, 0);
					}
//					System.out.println("Default selected: " + text);
					returnVal.setQuery(combo.getText());
					returnVal.setQuery(text);
					shell.dispose();
				}  });
			
			// we want these with most recent (last in Sequence) first
			for (int i = (previousQueries.size() - 1); i >= 0; i--) {
				if (i == (previousQueries.size() - 1)) {
					combo.setText(previousQueries.get(i).toString());
				}
				combo.add(previousQueries.get(i).toString());
			}

			shell.pack();
			shell.open();
			shell.setVisible(true);
			shell.setFocus();
			while (!shell.isDisposed()) {
				if (display != null && !display.readAndDispatch()) {
					display.sleep();
				}
			}
			query = returnVal.getQuery();
			boolean showNS = true;
			if (returnVal.isFqn() != showNamespaces) {
				// this changed
				showNS = returnVal.isFqn();
			}
			else {
	        	IPreferencesService service = Platform.getPreferencesService();
	        	showNS = service.getBoolean("com.ge.research.sadl.Sadl", "vnamespacesInQueryResults", true, null);
			}
			ConfigurationItem ci = new ConfigurationItem(CONSOLE);
			NameValuePair nvp = new NameValuePair(pSHOWNAMESPACES, showNS);
			nvp.setConfigType(ConfigurationType.SingleValue);
			ci.addNameValuePair(nvp);
			try {
				visitor.updateConfiguration(ci);
			} catch (ConfigurationException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (MalformedURLException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}	
		}
		else {
			Shell shell = new Shell();
			InputDialog dlg = new InputDialog(
				shell,
				"Run Query",
				"SADL or SPARQL Query?",
				query,
				null);
			dlg.open();
			if (dlg.getReturnCode() != Window.OK) {
				return;
			}
			query = dlg.getValue();
		}
		if (query != null && query.length() > 0) {
			if (query.toLowerCase().contains("select") && query.contains("?") && query.contains("{")) {
				if (!query.trim().startsWith("\"") && !query.trim().endsWith("\"")) {
					query = query.replace("\"", "\\\"");
					query = "\"" + query + "\"";
				}	
			}
			String augmentedQuery = query;
			if (!query.startsWith("Ask:")) {
				augmentedQuery = "Ask: " + query;
			}
			if (!query.endsWith(".")) {
				augmentedQuery += " .\n";
			}
			
			final String finalQuery = query;
			final List<Object> finalPreviousQueries = previousQueries;
			SadlConsole.writeToConsole(MessageType.INFO, "Query is: " + query + "\n");
			try {
				Query queryObj = parseQuery(resourceSet, augmentedQuery);
				final com.ge.research.sadl.model.gp.Query qif = visitor.translateQuery(queryObj);
		  		Job runTestJob = new Job("Run Tests") {
		  			
		  			@Override
		  			protected void canceling() {
		  				try {
		  					visitor.getConfigurationMgr((String)null).setInferenceCanceled(true);
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
		  				visitor.runQuery(modelName, qif);
		  				SadlConsole.displayMessages(visitor);
						IFile file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(testFilePath);
						if (file == null) {
							file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(testFilePath.toPortableString()));
						}
						SadlConsole.writeToConsoleWithHyperlink(MessageType.INFO, "Query execution completed.\n", -1, -1, 
								file, -1, -1, 3);
						if (finalQuery != null && finalQuery.length() > 0 && 
								(finalPreviousQueries == null || finalPreviousQueries.size() == 0 || !finalPreviousQueries.contains(finalQuery))) {
							try {
								ConfigurationItem ci = new ConfigurationItem(ADHOCQUERY);
								NameValuePair nvp = new NameValuePair(pQUERY, finalQuery);
								nvp.setConfigType(ConfigurationType.Sequence);
								ci.addNameValuePair(nvp);
								visitor.addConfiguration(ci);
								IConfigurationManagerForIDE cm = null;
								cm = visitor.getConfigurationMgr((String)null);
								if (cm != null) {
									//TODO this should be done with a listener but I'm having trouble for plugin close awc 1/10/2011
									cm.saveConfiguration();
								}
							} catch (ConfigurationException e1) {
								// TODO Auto-generated catch block
								e1.printStackTrace();
							} catch (MalformedURLException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							} catch (URISyntaxException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							} catch (IOException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}				
						}

						return Status.OK_STATUS;
		  			}
		  			
		  		};
				runTestJob.schedule();
			}
			catch (NullPointerException e) {
				e.printStackTrace();
				IFile file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(testFilePath);
				if (file == null) {
					file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(testFilePath.toPortableString()));
				}
				SadlConsole.writeToConsoleWithHyperlink(MessageType.INFO, "Null pointer Exception\n", -1, -1, 
						file, -1, -1, 3);
			}
			catch (Exception e) {
				// try as SPARQL query
				visitor.runQuery(modelName, query);
				SadlConsole.writeToConsole(MessageType.ERROR, e.getLocalizedMessage() + "\n");
			}
		}

	}

	private Query parseQuery (XtextResourceSet resourceSet, String queryString) {
		Query query = null;

		// Parse string just to test the syntax
		IParseResult result = parser.parse(grammar.getQueryRule(), new StringReader(queryString));
		if (result.hasSyntaxErrors()) {
			Iterable<INode> errors = result.getSyntaxErrors();
			for (INode nodeWithError : errors) {
				SadlConsole.writeToConsole(MessageType.ERROR, nodeWithError.getSyntaxErrorMessage().getMessage() + "\n");
			}
//			SadlActivator.getInstance().getLog().log(new Status(IStatus.ERROR, SadlActivator.getInstance().getBundle().getSymbolicName(), "Could not parse query '"+queryString+"'"));
		}

		Resource res = resourceSet.createResource(URI.createURI("http://com.ge.grc.sadl/__runtimequery.sadl"));
		StringBuilder resContent = new StringBuilder();
		resContent.append("uri 'http://com.ge.grc.sadl/__runtimequery'\n").append(queryString);
		try {
			res.load(new StringInputStream(resContent.toString()), null);
			Model model = (Model) res.getContents().get(0);
			query = (Query) model.getElements().get(0);
		} catch (Exception e) {
			SadlActivator.getInstance().getLog().log(new Status(IStatus.ERROR, SadlActivator.getInstance().getBundle().getSymbolicName(), "Could not parse query '"+queryString+"'", e));
		}

		EcoreUtil.resolveAll(resourceSet);
		return query;
	}
}
