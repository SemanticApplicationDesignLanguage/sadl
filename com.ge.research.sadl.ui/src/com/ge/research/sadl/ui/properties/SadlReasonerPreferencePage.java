/************************************************************************
 * Copyright 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.3 $ Last modified on   $Date: 2014/11/03 20:13:28 $
 ***********************************************************************/

package com.ge.research.sadl.ui.properties;

import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.ITranslator;

import com.ge.research.sadl.reasoner.IReasoner;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.ResourceManager;
import com.google.inject.Inject;

/**
 * The preference page that holds SADL reasoner preferences.
 */
public class SadlReasonerPrefrencePage extends PreferencePage implements IWorkbenchPreferencePage,
																		IWorkbenchPropertyPage {

	private static final Logger logger = LoggerFactory.getLogger(SadlReasonerPrefrencePage.class);

	private IWorkbench workbench;
	private IProject project;
	private IConfigurationManagerForIDE configurationManager;
	protected Button internal;
	protected Button external;
	protected Table table;
	protected CheckboxTableViewer tableViewer;
	protected Button edit;
	protected Label location;
	protected Label parameters;	
	protected IReasoner checkedReasoner;
	protected List<IReasoner> reasoners = null;
	
	protected Table translatorTable;
	protected List<ITranslator> allTranslators = null;
	protected CheckboxTableViewer translatorTableViewer;
	protected Button editTranslator;
	protected ITranslator checkedTranslator;
	protected List<ITranslator> translators = null;

	class ReasonerContentProvider implements IStructuredContentProvider {
		public Object[] getElements(Object inputElement) {
			return reasoners.toArray();
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// do nothing
		}

		public void dispose() {
			// do nothing
		}
	}

	class ReasonerTableLabelProvider implements ITableLabelProvider {
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		public String getColumnText(Object element, int columnIndex) {
			IReasoner reasoner = (IReasoner) element;
			// return notNull(reasoner.getClass().getName());
			return notNull(reasoner.getConfigurationCategory());
		}

		protected String notNull(String s) {
			if (s == null)
				return ""; //$NON-NLS-1$
			return s;
		}

		public boolean isLabelProperty(Object element, String property) {
			return false;
		}

		public void addListener(ILabelProviderListener listener) {
			// do nothing
		}

		public void removeListener(ILabelProviderListener listener) {
			// do nothing
		}

		public void dispose() {
			// do nothing
		}
	}

	class TranslatorContentProvider implements IStructuredContentProvider {
		public Object[] getElements(Object inputElement) {
			return translators.toArray();
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// do nothing
		}

		public void dispose() {
			// do nothing
		}
	}

	class TranslatorTableLabelProvider implements ITableLabelProvider {
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		public String getColumnText(Object element, int columnIndex) {
			ITranslator translator = (ITranslator) element;
			return translator.getConfigurationCategory();
			// return notNull(translator.getClass().getName());
		}

		protected String notNull(String s) {
			if (s == null)
				return ""; //$NON-NLS-1$
			return s;
		}

		public boolean isLabelProperty(Object element, String property) {
			return false;
		}

		public void addListener(ILabelProviderListener listener) {
			// do nothing
		}

		public void removeListener(ILabelProviderListener listener) {
			// do nothing
		}

		public void dispose() {
			// do nothing
		}
	}

	/**
	 * SadlReasonerPrefrencePage constructor comment.
	 */
	public SadlReasonerPrefrencePage() {
		super();
		allTranslators = ConfigurationManagerForIDE.getAvailableTranslators();
		translators = allTranslators;
	}

	/**
	 * Create the preference options.
	 * 
	 * @param parent
	 *            org.eclipse.swt.widgets.Composite
	 * @return org.eclipse.swt.widgets.Control
	 */
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.horizontalSpacing = convertHorizontalDLUsToPixels(4);
		layout.verticalSpacing = convertVerticalDLUsToPixels(3);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		composite.setLayout(layout);
		GridData data = new GridData(SWT.FILL, SWT.FILL, true, false);
		composite.setLayoutData(data);

		Label label = new Label(composite, SWT.WRAP);
		label.setText(Messages.preferenceReasonerDescription);
		data = new GridData(SWT.FILL, SWT.NONE, false, false);
		data.horizontalSpan = 2;
		data.widthHint = 275;
		label.setLayoutData(data);

		label = new Label(composite, SWT.NONE);
		label.setText(Messages.reasonerList);
		data = new GridData(SWT.FILL, SWT.CENTER, true, false);
		data.horizontalSpan = 2;
		label.setLayoutData(data);

		table = new Table(composite, SWT.CHECK | SWT.BORDER | SWT.V_SCROLL
				| SWT.H_SCROLL | SWT.SINGLE | SWT.FULL_SELECTION);
		data = new GridData(SWT.FILL, SWT.FILL, true, true);
		table.setLayoutData(data);
		table.setHeaderVisible(false);
		table.setLinesVisible(false);

		TableLayout tableLayout = new TableLayout();
		new TableColumn(table, SWT.NONE);
		tableLayout.addColumnData(new ColumnWeightData(100));
		table.setLayout(tableLayout);

		tableViewer = new CheckboxTableViewer(table);
		tableViewer.setContentProvider(new ReasonerContentProvider());
		tableViewer.setLabelProvider(new ReasonerTableLabelProvider());
		tableViewer.setInput("root");

		// un-check all other elements that might be checked and leave only the
		// element checked to remain checked since one can only chose one
		// reasoner at a time to be current.
		tableViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent e) {
				checkNewDefaultReasoner(e.getElement());
				checkedReasoner = (IReasoner) e.getElement();
				translators = getAvailableTranslatorsForReasoner(checkedReasoner);
				if (translatorTableViewer != null) {
					translatorTableViewer.refresh(true, false);
				}
				// if no other reasoners are checked, don't allow the single one
				// currently checked to become unchecked, and lose a current
				// reasoner. That is, don't permit un-checking if no other item
				// is checked which is supposed to be the case.
				Object[] obj = tableViewer.getCheckedElements();
				if (obj.length == 0)
					tableViewer.setChecked(e.getElement(), true);
			}
		});

		// set a default, checked reasoner based on the current reasoner. If there
		// is not a current reasoner, but the first item exists, use that instead.
		// This will work currently until workbench shutdown, because current
		// reasoner is not yet persisted.
		String reasonerName = null;
		try {
			reasonerName = configurationManager.getReasonerClassName();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		checkedReasoner = null;
		tableViewer.setAllChecked(true);
		Object[] obj = tableViewer.getCheckedElements();
		for (int i=0; i<obj.length; i++) {
			if (reasonerName != null &&
				((IReasoner)obj[i]).getClass().getName().equals(reasonerName)) {
				checkedReasoner = (IReasoner) obj[i];
				break;
			}
		}
		tableViewer.setAllChecked(false);
		if (checkedReasoner != null)
			tableViewer.setChecked(checkedReasoner, true);
		else {
			Object o = tableViewer.getElementAt(0);
			if (o != null) {
				tableViewer.setChecked(o, true);
				checkedReasoner = (IReasoner) o;
			}
		}
		translators = getAvailableTranslatorsForReasoner(checkedReasoner);

		tableViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection sele = ((IStructuredSelection) tableViewer.getSelection());
				boolean sel = sele.getFirstElement() != null && (sele.getFirstElement() instanceof IReasoner);
				edit.setEnabled(sel);
				}
			});

		tableViewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				IStructuredSelection sel = ((IStructuredSelection) tableViewer.getSelection());
				Object firstElem = sel.getFirstElement();
				if (firstElem != null && (firstElem instanceof IReasoner)) {
					IReasoner r = (IReasoner) sel.getFirstElement();
					String rc = r.getConfigurationCategory();
					Map<String, ConfigurationOption> config = r.getReasonerConfigurationOptions();
					ReasonerConfigurationDialog dialog = new ReasonerConfigurationDialog(
							getShell(), rc, config, configurationManager);
					int dialogReturnButton = dialog.open();
				}
			}
		});

		Composite buttonComp = new Composite(composite, SWT.NONE);
		layout = new GridLayout();
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = convertVerticalDLUsToPixels(3);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		layout.numColumns = 1;
		buttonComp.setLayout(layout);
		data = new GridData(GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_FILL);
		buttonComp.setLayoutData(data);

		edit = createButton(buttonComp, Messages.edit);
		edit.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				IStructuredSelection sel = ((IStructuredSelection) tableViewer.getSelection());
				IReasoner r = (IReasoner) sel.getFirstElement();
				String rc = r.getConfigurationCategory();
				Map<String, ConfigurationOption> config = r.getReasonerConfigurationOptions();
				// addTestingOptions(config);
				ReasonerConfigurationDialog dialog = new ReasonerConfigurationDialog(
						getShell(), rc, config, configurationManager);
				int dialogReturnButton = dialog.open();
			}
		});
		tableViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent e) {
				checkNewDefaultReasoner(e.getElement());
				checkedReasoner = (IReasoner) e.getElement();
				translators = getAvailableTranslatorsForReasoner(checkedReasoner);
				if (translatorTableViewer != null) {
					translatorTableViewer.refresh(true, false);
				}
			}
		});

		IStructuredSelection sele = ((IStructuredSelection) tableViewer.getSelection());
		boolean sel = sele.getFirstElement() != null &&	(sele.getFirstElement() instanceof IReasoner);
		edit.setEnabled(sel);

		// Translator code starts here
		label = new Label(composite, SWT.NONE);
		label.setText(Messages.translatorList);
		data = new GridData(SWT.FILL, SWT.CENTER, true, false);
		data.horizontalSpan = 2;
		label.setLayoutData(data);

		translatorTable = new Table(composite, SWT.CHECK | SWT.BORDER | SWT.V_SCROLL
				| SWT.H_SCROLL | SWT.SINGLE | SWT.FULL_SELECTION);
		data = new GridData(SWT.FILL, SWT.FILL, true, true);
		translatorTable.setLayoutData(data);
		translatorTable.setHeaderVisible(false);
		translatorTable.setLinesVisible(false);

		TableLayout translatorTableLayout = new TableLayout();
		new TableColumn(translatorTable, SWT.NONE);
		translatorTableLayout.addColumnData(new ColumnWeightData(100));
		translatorTable.setLayout(translatorTableLayout);

		translatorTableViewer = new CheckboxTableViewer(translatorTable);
		translatorTableViewer.setContentProvider(new TranslatorContentProvider());
		translatorTableViewer.setLabelProvider(new TranslatorTableLabelProvider());
		translatorTableViewer.setInput("root");

		// un-check all other elements that might be checked and leave only the
		// element checked to remain checked since one can only chose one
		// translator at a time to be current.
		translatorTableViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent e) {
				checkNewDefaultTranslator(e.getElement());
				checkedTranslator = (ITranslator) e.getElement();			
				// if no other translators are checked, don't allow the single one
				// currently checked to become unchecked, and lose a current
				// translator. That is, don't permit un-checking if no other item
				// is checked which is supposed to be the case.
				Object[] obj = translatorTableViewer.getCheckedElements();
				if (obj.length == 0)
					translatorTableViewer.setChecked(e.getElement(), true);
			}
		});

		// set a default, checked translator based on the current reasoner. If there
		// is not a current reasoner, but the first item exists, use that instead.
		// This will work currently until workbench shutdown, because current
		// reasoner is not yet persisted.
		String translatorName = null;
		try {
			translatorName = configurationManager.getTranslatorClassName();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		checkedTranslator = null;
		translatorTableViewer.setAllChecked(true);
		obj = translatorTableViewer.getCheckedElements();
		for (int i=0; i<obj.length; i++) {
			if (translatorName != null &&
				((ITranslator)obj[i]).getClass().getName().equals(translatorName)) {
				checkedTranslator = (ITranslator) obj[i];
				break;
				}
			}
		translatorTableViewer.setAllChecked(false);
		if (checkedTranslator != null)
			translatorTableViewer.setChecked(checkedTranslator, true);
		else {
			Object o = translatorTableViewer.getElementAt(0);
			if (o != null) {
				translatorTableViewer.setChecked(o, true);
				checkedTranslator = (ITranslator) o;
			}
		}

		translatorTableViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection sele = ((IStructuredSelection) translatorTableViewer.getSelection());
				boolean sel = sele.getFirstElement() != null && (sele.getFirstElement() instanceof ITranslator);
				editTranslator.setEnabled(sel);
				}
			});

		translatorTableViewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				IStructuredSelection sel = ((IStructuredSelection) translatorTableViewer.getSelection());
				Object firstElem = sel.getFirstElement();
				if (firstElem != null && (firstElem instanceof ITranslator)) {
					ITranslator t = (ITranslator) sel.getFirstElement();
					String tc = t.getConfigurationCategory();
					Map<String, ConfigurationOption> config = t.getTranslatorConfigurationOptions();
					TranslatorConfigurationDialog dialog = new TranslatorConfigurationDialog(
							getShell(), t, configurationManager);
					int dialogReturnButton = dialog.open();
				}
			}
		});

		Composite buttonCompTrans = new Composite(composite, SWT.NONE);
		GridLayout translatorEditBtnLayout = new GridLayout();
		translatorEditBtnLayout.horizontalSpacing = 0;
		translatorEditBtnLayout.verticalSpacing = convertVerticalDLUsToPixels(3);
		translatorEditBtnLayout.marginWidth = 0;
		translatorEditBtnLayout.marginHeight = 0;
		translatorEditBtnLayout.numColumns = 1;
		buttonCompTrans.setLayout(translatorEditBtnLayout);
		data = new GridData(GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_FILL);
		buttonCompTrans.setLayoutData(data);

		editTranslator = createButton(buttonCompTrans, Messages.edit);
		editTranslator.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				IStructuredSelection sel = ((IStructuredSelection) translatorTableViewer.getSelection());
				ITranslator t = (ITranslator) sel.getFirstElement();
				String tc = t.getConfigurationCategory();
				Map<String, ConfigurationOption> config = t.getTranslatorConfigurationOptions();
				// addTestingOptions(config);
				TranslatorConfigurationDialog dialog = new TranslatorConfigurationDialog(
						getShell(), t, configurationManager);
				int dialogReturnButton = dialog.open();
			}
		});
		translatorTableViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(CheckStateChangedEvent e) {
				checkNewDefaultTranslator(e.getElement());
				checkedTranslator = (ITranslator) e.getElement();
				}
		});

		IStructuredSelection selet = ((IStructuredSelection) translatorTableViewer.getSelection());
		boolean selt = selet.getFirstElement() != null &&	(selet.getFirstElement() instanceof ITranslator);
		editTranslator.setEnabled(selt);
		translatorTableViewer.refresh(true, false);

		// Translator code ends here
		Dialog.applyDialogFont(composite);

		return composite;
	}

	/**
	 * Initializes this preference page using the passed workbench.
	 * 
	 * @param workbench
	 *            the current workbench
	 */
	public void init(IWorkbench workbench) {
		this.workbench = workbench;
		String configDir = project.getLocation().append(ResourceManager.OWLDIR).toPortableString(); 
		try {
			if (configurationManager == null) {
				configurationManager = new ConfigurationManagerForIDE(configDir, ConfigurationManagerForIDE.getOWLFormat());
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * 
	 */
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible)
			setTitle(Messages.preferenceReasonerTitle);
	}

	protected Object getSelection(ISelection sel2) {
		IStructuredSelection sel = (IStructuredSelection) sel2;
		return sel.getFirstElement();
	}

	// Un-check all the items except the current one that was just checked
	protected void checkNewDefaultReasoner(Object reasoner) {
		TableItem[] children = tableViewer.getTable().getItems();
		for (int i = 0; i < children.length; i++) {
			TableItem item = children[i];
			if (!(item.getData().equals(reasoner)))
				item.setChecked(false);
		}
	}

	// Un-check all the translators except the current one that was just checked
	protected void checkNewDefaultTranslator(Object translator) {
		TableItem[] children = translatorTableViewer.getTable().getItems();
		for (int i = 0; i < children.length; i++) {
			TableItem item = children[i];
			if (!(item.getData().equals(translator)))
				item.setChecked(false);
		}
	}

	/**
	 * Performs special processing when this page's Defaults button has been
	 * pressed.
	 */
	protected void performDefaults() {
		String reasonerName = null;
		try {
			reasonerName = configurationManager.getReasonerClassName();
		} catch (ConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		checkedReasoner = null;
		Object[] obj = tableViewer.getCheckedElements();
		for (int i=0; i<obj.length; i++) {
			tableViewer.setChecked(obj, false);
			if (reasonerName != null &&
				((IReasoner)obj[i]).getClass().getName().equals(reasonerName)) {
				checkedReasoner = (IReasoner) obj[i];
				break;
			}
		}
		if (checkedReasoner != null)
			tableViewer.setChecked(checkedReasoner, true);
		else {
			Object o = tableViewer.getElementAt(0);
			if (o != null)
				tableViewer.setChecked(o, true);
		}
		
		super.performDefaults();
	}

	/**
	 * Method declared on IPreferencePage. Subclasses should override
	 */
	public boolean performOk() {
		boolean success = true;
		IPreferenceStore ps = this.getPreferenceStore();
		try {
			if (checkedReasoner != null) {
				ps.setDefault(this.getQualifier()+".ReasonerName", checkedReasoner.getClass().getName());
				saveReasonerSpec();
			} else {
				ps.setDefault(this.getQualifier()+".ReasonerName", "");
				saveReasonerSpec();
			}
		}
		catch (Throwable t) {
			t.printStackTrace();
			String msg = t.getLocalizedMessage();
			if (msg == null) {
				msg = t.getClass().getSimpleName();
			}
			SadlConsole.writeToConsole(MessageType.ERROR, "Error saving Reasoner selection: " + msg + "\n");
			success = false;
		}
		try {
			if (checkedTranslator != null) {
				ps.setDefault(this.getQualifier()+".TranslatorName", checkedTranslator.getClass().getName());
				saveTranslatorSpec();
			}
			else {
				ps.setDefault(this.getQualifier()+".TranslatorName", "");
				saveTranslatorSpec();
			}
		} catch (Throwable t) {
			t.printStackTrace();
			String msg = t.getLocalizedMessage();
			if (msg == null) {
				msg = t.getClass().getSimpleName();
			}
			SadlConsole.writeToConsole(MessageType.ERROR, "Error saving Translator selection: " + msg + "\n");
			success = false;;
		}
		return success;
	}
	
	public boolean performCancel() {
		return super.performCancel();
	}

	/**
	 * Create a new button with the standard size.
	 * 
	 * @param comp a component to add the button to
	 * @param label the button label
	 * @return a button
	 */
	public static Button createButton(Composite comp, String label) {
		Button b = new Button(comp, SWT.PUSH);
		b.setText(label);
		GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_BEGINNING);
		b.setLayoutData(data);
		return b;
	}

	protected IWorkbench getWorkbench() {
		return workbench;
	}
	
	public IAdaptable getElement() {
		return project;
	}

	public void setElement(IAdaptable element) {
		this.project = (IProject) element.getAdapter(IProject.class); 
		String configDir = project.getLocation().append(ResourceManager.OWLDIR).toPortableString(); 
		try {
			if (configurationManager == null) {
				configurationManager = new ConfigurationManagerForIDE(configDir, ConfigurationManagerForIDE.getOWLFormat());
			}
			reasoners = ConfigurationManagerForIDE.getAvailableReasoners();
			logger.debug("Number of reasoners: "+reasoners.size());
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public boolean isPropertyPage() {
		return project != null;
	}

	private IProject currentProject() {
		if (project == null)
		 	throw new IllegalStateException("Not a property page case, but current project was requested."); //$NON-NLS-1$
		 	return project;
		} 
	
	@Inject
	private IPreferenceStoreAccess preferenceStoreAccess;

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		if (isPropertyPage()) {
			return preferenceStoreAccess.getWritablePreferenceStore(currentProject());
		}
		return preferenceStoreAccess.getWritablePreferenceStore();
	}

	/**
	 * @return the qualifier used to look up the preference node of the
	 *         configured preferenceStore
	 */
	protected String getQualifier() {
		return "org.eclipse.xtext.builder"; // TODO: KTH: Is this the right bundle? Code referenced org.eclipse.xtext.builder.internal.Activator here.
		// removed due to Illegal Access
		// return Activator.getDefault().getBundle().getSymbolicName();
	}

	private void saveReasonerSpec() {
		this.configurationManager.clearReasoner();	// need to remove any residual reasoner so it won't "stick"
		this.configurationManager.setReasonerClassName(checkedReasoner != null ? checkedReasoner.getClass().getName() : null);
		this.configurationManager.saveConfiguration();
	}
	
	private void saveTranslatorSpec() throws ConfigurationException {
		this.configurationManager.setTranslatorClassName(checkedTranslator != null ? checkedTranslator.getClass().getName() : null);
		this.configurationManager.saveConfiguration();
	}


	/**
	 * Method to get a list of all the available translators for a specific reasoner
	 * @return A list of all available translators
	 * @throws ConfigurationException 
	 */
	public List<ITranslator> getAvailableTranslatorsForReasoner(IReasoner reasoner) {
		List<ITranslator> rval = new ArrayList<ITranslator>();
		for (ITranslator translator : allTranslators) {
			String family = translator.getReasonerFamily();
			if (family != null && family.equals(reasoner.getReasonerFamily())) {
//				if (reasoner.getConfigurationCategory().equalsIgnoreCase("jena") &&
//					translator.getConfigurationCategory().contains("Opt")) {
//					 continue;
//				}
				rval.add(translator);
			}			
		}
		return rval;
	}

	private	void addTestingOptions(Map<String, ConfigurationOption> config) {
		String[] category = new String[]{"Jena"};
		ConfigurationOption stringOption = new ConfigurationOption(category, "stringOption", "String option", "New string option");
		config.put("stringOption", stringOption);
		ConfigurationOption booleanOption = new ConfigurationOption(category, "booleanOption", "Boolean option", new Boolean(true));
		config.put("booleanOption", booleanOption);
		ConfigurationOption integerOption = new ConfigurationOption(category, "integerOption", "Integer option", new Integer(567));
		config.put("integerOption", integerOption);
		ConfigurationOption doubleOption = new ConfigurationOption(category, "doubleOption", "Double option", new Double(123.45));
		config.put("doubleOption", doubleOption);
	}

}
