package com.ge.research.sadl.ui.handlers;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.URIUtil;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
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
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.xtext.preferences.*;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.ConfigurationManagerForIdeFactory;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.external.ExternalEmfResource;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.ConfigurationType;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IReasoner;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.reasoner.TranslationException;
import com.ge.research.sadl.reasoner.utils.SadlUtils;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.ui.internal.SadlActivator;
import com.ge.research.sadl.utils.ResourceManager;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.hp.hpl.jena.assembler.exceptions.TransactionAbortedException;

public class RunQuery extends SadlActionHandler {

	private static final String pSHOWNAMESPACES = "pShowNamespaces";
	private static final String[] CONSOLE = {"Console"};
	public String projectLocation;
	protected Process Process;

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

	public RunQuery() {
		super();
	}

	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = getValidTargetFileTypes();
			Object[] target = getCommandTarget(validTargetTypes);
			IProject project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}
			String owlFileName = null;

			Map<String,String> prefMap = getPreferences();
			if (trgtFile != null) {
				if (trgtFile.getFileExtension().equals("sadl")) {
					// run query on this model
	//				Resource res = prepareActionHandler(target[2]);
					SadlConsole.writeToConsole(MessageType.INFO, "Adhoc Query of '" + trgtFile.getFullPath().toPortableString() + "' requested.\n");
					
	//				final List<Issue> issues = new ArrayList<Issue>();
	//				processor.processAdhocQuery(res, new ValidationAcceptor(new IAcceptor<Issue>(){
	
	//					@Override
	//					public void accept(Issue t) {
	//						issues.add(t);
	//					}
						
	//				}),  new ProcessorContext(CancelIndicator.NullImpl,  preferenceProvider.getPreferenceValues(res)), query);
	//				if (issues.size() > 0) {
	//					for (Issue issue: issues) {
	//						output.writeToConsole(MessageType.ERROR, issue.getMessage() + "\n");
	//					}
	//				}
					owlFileName = trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
				}
				else if (trgtFile.getFileExtension().equals("owl")) {
					// run query on this model
	//				Resource res = prepareActionHandler(trgtFile);
					SadlConsole.writeToConsole(MessageType.INFO, "Adhoc Query of '" + trgtFile.getFullPath().toPortableString() + "' requested.\n");
					owlFileName = trgtFile.getFullPath().lastSegment();
				}
				String modelFolderUri = convertProjectRelativePathToAbsolutePath(project.getFullPath().append(ResourceManager.OWLDIR).toPortableString()); 
				final String format = ConfigurationManager.RDF_XML_ABBREV_FORMAT;
				IConfigurationManagerForIDE configMgr = ConfigurationManagerForIdeFactory.getConfigurationManagerForIDE(modelFolderUri, format);
				String query = getQuery(configMgr);
				new SadlUtils();
				query = SadlUtils.stripQuotes(query);
				File qf = new File(query);
				List<String> qlist = null;
				if (qf.exists()) {
					List<String[]> queries = new SadlUtils().parseQueries(qf);
					if (queries != null && queries.size() > 0) {
						qlist = new ArrayList<String>();
						for (int i = 0; i < queries.size(); i++) {
							qlist.add(queries.get(i)[1]);
						}
					}
				}
				else {
					qlist = new ArrayList<String>();
					qlist.add(query);
				}
				{
					IReasoner reasoner = configMgr.getReasoner();
					if (!reasoner.isInitialized()) {
						reasoner.setConfigurationManager(configMgr);
						String modelName = configMgr.getPublicUriFromActualUrl(new SadlUtils().fileNameToFileUrl(modelFolderUri + "/" + owlFileName));
						reasoner.initializeReasoner(modelFolderUri, modelName, format);
					}
					if (reasoner != null) {	
						int qidx = 0;
						while (query != null) {
							if (qlist != null) {
								if (qidx < qlist.size()) {
									query = qlist.get(qidx++);
								}
								else {
									break;
								}
							}
							try {
								String currentQuery = reasoner.prepareQuery(query);
								ResultSet rs = reasoner.ask(currentQuery);
								if (rs != null) {
									SadlConsole.getInstance().writeToConsole(MessageType.INFO, rs.toStringWithIndent(5));
								}
								else {
									SadlConsole.getInstance().writeToConsole(MessageType.WARN, "Query '" + currentQuery + "' returned no results\n");
								}
							}
							catch (Throwable t) {
								System.err.println("Error processing query '" + query + "'");
								System.err.println("   " + t.getMessage());
							}
						}
						configMgr.clearReasoner();
					}
				}
//				else if (trgtFile.getName().endsWith("test")) {
//					// run test suite
//					output.writeToConsole(MessageType.INFO, "Testing of suite '" +  trgtFile.getName() + "' requested.\n");
//				}
			}
			else {
				throw new TranslationException("Please select a target for querying");
			}
		}
		catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {

		}

		return event;
	}

	public static final String pQUERY = "pQuery";
	public static final String[] ADHOCQUERY = {"AdHocQuery"};

	private String getQuery(IConfigurationManagerForIDE configMgr) throws ConfigurationException {
		String query = null;
		List<Object> previousQueries = null;
		List<ConfigurationItem> config = configMgr.getConfiguration(ADHOCQUERY, false);
		if (config != null && config.size() > 0) {
			previousQueries = config.get(0).getAllValuesOfName(pQUERY);
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
				cil = configMgr.getConfiguration(CONSOLE, false);
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
//	        	IPreferencesService service = Platform.getPreferencesService();
	        	showNS = false; //service.getBoolean("com.ge.research.sadl.Sadl", "vnamespacesInQueryResults", true, null);
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
				return null;
			}
			query = dlg.getValue();
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
			}
		}
		if (query != null) {
			ConfigurationItem ci = new ConfigurationItem(ADHOCQUERY);
			NameValuePair nvp = ci.new NameValuePair(pQUERY, query);
			nvp.setConfigType(ConfigurationType.Sequence);
			ci.addNameValuePair(nvp);
			configMgr.addConfiguration(ci);
		}
		return query;
	}

	@Override
	protected String[] getValidTargetFileTypes() {
		String[] types = {"sadl","owl"};
		return types;
	}

}