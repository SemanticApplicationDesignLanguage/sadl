/*******************************************************************************
 * Copyright (c) 2000, 2010 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Barry Hathaway - modified for CSV file imports
 *******************************************************************************/
package com.ge.research.sadl.ui.imports;

import java.io.File;
import java.io.FileNotFoundException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardResourceImportPage;
import org.eclipse.ui.internal.ide.dialogs.RelativePathVariableGroup;
import org.eclipse.ui.internal.ide.filesystem.FileSystemStructureProvider;
import org.eclipse.ui.internal.misc.StatusUtil;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.statushandlers.StatusManager;

import com.ge.research.sadl.jena.importer.CsvImporter;


/**
 *	Page 1 of the base resource import-CSV-from-file Wizard
 */
public class CSVFileResourceImportPage1 extends WizardResourceImportPage
        implements Listener {
    // widgets
    protected Button firstRowVarButton;
    protected Button debugOutputButton;
    protected Button overwriteExistingResourcesCheckbox;
    protected Button createTopLevelFolderCheckbox;    
    protected RelativePathVariableGroup relativePathVariableGroup;    
    protected String pathVariable;    
    protected Button sourceBrowseButton;
    protected Button selectTypesButton;
    protected Button selectAllButton;
    protected Button deselectAllButton;
	private Button browseButton = null;
	private Text csvFileText = null;
	private Text namespaceText = null;
	private ListViewer importsViewer;
	private Button addButton;
	private Button editButton;
	private Button removeButton;
    protected Button templateBrowseButton;
	private Text templateFileText = null;
	private String template;
	private String uri;
	private boolean namespaceExplicitlySet = false;
	protected Button importAsOwlCheckbox;
	protected Button importAsSadlCheckbox;
	protected Button restoreCheckbox;

    //A boolean to indicate if the user has typed anything
    private boolean entryChanged = false;    
    private FileSystemStructureProvider fileSystemStructureProvider = new FileSystemStructureProvider();
    private List<String> imports = new ArrayList<String>();

    // dialog store id constants
    private final static String STORE_SOURCE_CSV_FILE_ID = "CSVFileResourceImportPage1.SourceCsvFile";//$NON-NLS-1$
    private final static String STORE_HEADER_ROW_ID = "CSVFileResourceImportPage1.HasHeaderRow";
    private final static String STORE_IMPORT_MODEL_NAMESPACE_ID = "CSVFileResourceImportPage1.ImportModelNamespace";
    private final static String STORE_TEMPLATE_FILE_ID = "CSVFileResourceImportPage1.TemplateFile";
    private final static String STORE_IMPORTS_ID = "CSVFileResourceImportPage1.Imports";
    private final static String STORE_DESTINATION_FOLDER_ID = "CSVFileResourceImportPage1.DestinationFolder";
    private final static String STORE_OVERWRITE_EXISTING_RESOURCES_ID = "CSVFileResourceImportPage1.OverwriteExistingResources";//$NON-NLS-1$
    private final static String STORE_CREATE_CONTAINER_STRUCTURE_ID = "CSVFileResourceImportPage1.CreateTopLevelFolder";//$NON-NLS-1$
    private final static String STORE_IMPORT_AS_OWL_ID = "CSVFileResourceImportPage1.ImportAsOwl";//$NON-NLS-1$
    private final static String STORE_IMPORT_AS_SADL_ID = "CSVFileResourceImportPage1.ImportAsSadl";//$NON-NLS-1$
    private static final String SELECT_SOURCE_TITLE = SADLImportMessages.CSVImport_source_dialog_title;
    protected static final String SOURCE_EMPTY_MESSAGE = SADLImportMessages.FileImport_sourceEmpty;

    /**
     *	Creates an instance of this class
     */
    protected CSVFileResourceImportPage1(String name,
            IWorkbench aWorkbench, IStructuredSelection selection) {
        super(name, selection);
    }

    /**
     *	Creates an instance of this class
     *
     * @param aWorkbench IWorkbench
     * @param selection IStructuredSelection
     */
    public CSVFileResourceImportPage1(IWorkbench aWorkbench,
            IStructuredSelection selection) {
        this("CSVFileImportPage1", aWorkbench, selection);//$NON-NLS-1$
        setTitle(SADLImportMessages.CSVImport_Page_Title);
        setDescription(SADLImportMessages.CSVImport_Page_Description);
    }


    /* (non-Javadoc)
     * Method declared on IDialogPage.
     */
    public void createControl(Composite parent) {
        super.createControl(parent);
        setMessage(SADLImportMessages.CSVImport_csv_field_message);
        validateSourceGroup();
//        PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(),
//                IDataTransferHelpContextIds.FILE_SYSTEM_IMPORT_WIZARD_PAGE);
    }

    /**
     *	Create the import options specification widgets.
     */
    protected void createOptionsGroupButtons(Group optionsGroup) {

        // overwrite... checkbox
        overwriteExistingResourcesCheckbox = new Button(optionsGroup, SWT.CHECK);
        overwriteExistingResourcesCheckbox.setFont(optionsGroup.getFont());
        overwriteExistingResourcesCheckbox.setText(SADLImportMessages.FileImport_overwriteExisting);

        // create top-level folder check box
        createTopLevelFolderCheckbox= new Button(optionsGroup, SWT.CHECK);
        createTopLevelFolderCheckbox.setFont(optionsGroup.getFont());
        createTopLevelFolderCheckbox.setText(SADLImportMessages.FileImport_createTopLevel);
        createTopLevelFolderCheckbox.setSelection(false);
        createTopLevelFolderCheckbox.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		updateWidgetEnablements();
        	}
        });
        // create import as checkboxes
        importAsOwlCheckbox = new Button(optionsGroup, SWT.CHECK);
        importAsOwlCheckbox.setFont(optionsGroup.getFont());
        importAsOwlCheckbox.setText(SADLImportMessages.CSVImport_as_owl_label);
        importAsOwlCheckbox.setSelection(true);
        importAsOwlCheckbox.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		importAsSadlCheckbox.setSelection(false);
        	}
        });
        importAsSadlCheckbox = new Button(optionsGroup, SWT.CHECK);
        importAsSadlCheckbox.setFont(optionsGroup.getFont());
        importAsSadlCheckbox.setText(SADLImportMessages.CSVImport_as_sadl_label);
        importAsSadlCheckbox.setSelection(false);
        importAsSadlCheckbox.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		importAsOwlCheckbox.setSelection(false);
        	}
        });
		updateWidgetEnablements();
	}


    /**
     *	Create the group for creating the source CSV file input area
     */
    protected void createSourceCSVGroup(Composite parent) {
        Composite sourceCSVGroup = new Composite(parent, SWT.NONE);
		GridLayout fileLayout = new GridLayout();
		fileLayout = new GridLayout();
		fileLayout.numColumns = 3;
		fileLayout.marginHeight = 0;
		fileLayout.marginWidth = 0;
		fileLayout.makeColumnsEqualWidth = false;
		sourceCSVGroup.setLayout(fileLayout);
		GridData gridData = GridDataUtil.createHorizontalFill();
		sourceCSVGroup.setLayoutData(gridData);

        Label groupLabel = new Label(sourceCSVGroup, SWT.NONE);
        groupLabel.setText(SADLImportMessages.CSVImport_title);
        groupLabel.setFont(parent.getFont());

		csvFileText = new Text(sourceCSVGroup, SWT.BORDER); 
		gridData = GridDataUtil.createHorizontalFill();
		gridData.widthHint = 300;
		csvFileText.setLayoutData(gridData);
		csvFileText.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });
		csvFileText.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent e) {
                //Do nothing when getting focus
            }
            public void focusLost(FocusEvent e) {
            	String f = csvFileText.getText();
                if (f == null || f.isEmpty()) {
                	// Set enter csv file message
                	setMessage(SADLImportMessages.CSVImport_csv_field_message);
                } else if (validFileName(f)) {
                	// Clear enter csv file message
                	setErrorMessage(null);
                	setMessage(null);
                	// Populate namespace if necessary
                	String ns = namespaceText.getText();
                	if (ns == null || ns.isEmpty()) {
                    	prepopulateNamespace(f);                		
                	}
                } else {
                	// Display error message
                	setErrorMessage(SADLImportMessages.CSVImport_invalid_file_message);
                }
            }
        });

        // source browse button
        sourceBrowseButton = new Button(sourceCSVGroup, SWT.PUSH);
        sourceBrowseButton.setText(SADLImportMessages.DataTransfer_browse);
        sourceBrowseButton.addListener(SWT.Selection, this);
        sourceBrowseButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));
        sourceBrowseButton.setFont(parent.getFont());
        setButtonLayoutData(sourceBrowseButton);

        // first row contains headings button
        Composite optionComposite = new Composite(sourceCSVGroup, SWT.NONE);        
		GridLayout optionLayout = new GridLayout();
		optionLayout = new GridLayout();
		optionLayout.numColumns = 2;
		optionLayout.marginHeight = 0;
		optionLayout.marginWidth = 0;
		optionLayout.makeColumnsEqualWidth = false;
		optionComposite.setLayout(optionLayout);
		gridData = GridDataUtil.createHorizontalFill();
		gridData.horizontalSpan = 3;
		optionComposite.setLayoutData(gridData);
		
		firstRowVarButton = new Button(optionComposite, SWT.CHECK);
		firstRowVarButton.setSelection(true);
		Label firstRowLabel = new Label(optionComposite, SWT.NULL);
		firstRowLabel.setText(SADLImportMessages.CSVImport_Col_Header);
		debugOutputButton = new Button(optionComposite, SWT.CHECK);
		debugOutputButton.setSelection(false);
		Label debugOutputLabel = new Label(optionComposite, SWT.NULL);
		debugOutputLabel.setText(SADLImportMessages.CSVImport_Debug);
    }

    /**
     *	Create the group for creating the root directory
     */
    protected void createNamespaceGroup(Composite parent) {
        Composite namespaceGroup = new Composite(parent, SWT.NONE);
		GridLayout fileLayout = new GridLayout();
		fileLayout = new GridLayout();
		fileLayout.numColumns = 2;
		fileLayout.marginHeight = 0;
		fileLayout.marginWidth = 0;
		fileLayout.makeColumnsEqualWidth = false;
		namespaceGroup.setLayout(fileLayout);
		GridData gridData = GridDataUtil.createHorizontalFill();
		namespaceGroup.setLayoutData(gridData);

        Label groupLabel = new Label(namespaceGroup, SWT.NONE);
        groupLabel.setText(SADLImportMessages.CSVImport_namespace);
        groupLabel.setFont(parent.getFont());

		namespaceText = new Text(namespaceGroup, SWT.BORDER); 
		gridData = GridDataUtil.createHorizontalFill();
		gridData.widthHint = 300;
		namespaceText.setLayoutData(gridData);
		namespaceText.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });
		namespaceText.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent e) {
                //Do nothing when getting focus
            }
            public void focusLost(FocusEvent e) {
            	String f = namespaceText.getText();
                if (f == null || f.isEmpty()) {
                	// Set enter namespace message (namespace is optional for now)
                	setMessage(SADLImportMessages.CSVImport_namespace_field_message);
                } else if (validURI(f) || f.contains("/${project_name}")) {
                	// Clear enter namespace message
                	setErrorMessage(null);
                	setMessage(null);
                	uri = f;
                } else {
                	// Display error message
                	setErrorMessage(SADLImportMessages.CSVImport_invalid_namespace_message);
                }
            }
        });
    }
    

    /**
     * Create the imports composite
     */
    private void createImportsGroup(final Composite parent) {
    	Composite importsGroup = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.makeColumnsEqualWidth = false;
		importsGroup.setLayout(layout);
		GridData gridData = GridDataUtil.createHorizontalFill();
		importsGroup.setLayoutData(gridData);

		Label label = new Label(importsGroup, SWT.NONE);
        label.setText(SADLImportMessages.CSVImport_imports);
        GridData ldata = new GridData();
        ldata.horizontalSpan = 2;
        label.setLayoutData(ldata);

        importsViewer = new ListViewer(importsGroup);
        importsViewer.setComparator(new ViewerComparator());
        importsViewer.getControl().setFont(parent.getFont());
        importsViewer.setContentProvider(new ArrayContentProvider());
        importsViewer.setLabelProvider(new LabelProvider());
        importsViewer.setInput(imports);
        GridData data = new GridData(GridData.FILL_BOTH);
        data.horizontalSpan = 1;
        importsViewer.getControl().setLayoutData(data);
        importsViewer.addSelectionChangedListener(new ISelectionChangedListener() {
        	public void selectionChanged(SelectionChangedEvent event) {
        		IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        		if (selection.isEmpty()) {
        			editButton.setEnabled(false);
        			removeButton.setEnabled(false);
        			return;
        		}
        		boolean enabled = true;
        		List<String> elements = selection.toList();
        		editButton.setEnabled(enabled && selection.size() == 1);
        		removeButton.setEnabled(enabled);
        	}
        });
        
        Composite buttonArea = new Composite(importsGroup, SWT.NONE);
        GridLayout buttonLayout = new GridLayout(1, false);
        buttonLayout.marginHeight = buttonLayout.marginWidth = 0;
        buttonArea.setLayout(buttonLayout);
        GridData bdata = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
        buttonArea.setLayoutData(bdata);
        addButton = new Button(buttonArea, SWT.PUSH);
        addButton.setFont(parent.getFont());
        addButton.setText(SADLImportMessages.CSVImport_imports_add_label);
        addButton.setEnabled(false);
        setButtonLayoutData(addButton);
        addButton.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		Shell shell = parent.getShell();
        		String selectedImport = (String) ((IStructuredSelection) importsViewer.getSelection()).getFirstElement();
        		ImportsDialog dialog = new ImportsDialog(shell);
        		if (dialog.open() == Window.OK) {
        			String importValue = dialog.getImportValue();
    				imports.add(importValue);
       				importsViewer.refresh(false);
        		}
        	}
        });

        editButton = new Button(buttonArea, SWT.PUSH);
        editButton.setFont(parent.getFont());
        editButton.setText(SADLImportMessages.CSVImport_imports_edit_label);
        editButton.setEnabled(false);
        setButtonLayoutData(editButton);
        editButton.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		Shell shell = parent.getShell();
        		String selectedImport = (String) ((IStructuredSelection) importsViewer.getSelection()).getFirstElement();
        		ImportsDialog dialog = new ImportsDialog(shell);
        		dialog.setInitialValue(selectedImport);
        		if (dialog.open() == Window.OK) {
        			String importValue = dialog.getImportValue();
    				// remove the original import
    				imports.remove(selectedImport);
    				// add the new import
    				imports.add(importValue);
    				importsViewer.refresh(false);
        		}
        	}
       	});

        removeButton = new Button(buttonArea, SWT.PUSH);
        removeButton.setEnabled(false);
        removeButton.setText(SADLImportMessages.CSVImport_imports_remove_label);
        setButtonLayoutData(removeButton);
        removeButton.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent event) {
        		List<String> selectedImports = ((IStructuredSelection) importsViewer.getSelection()).toList();
        		MultiStatus result = new MultiStatus(PlatformUI.PLUGIN_ID, 0, new IStatus[0],
        				SADLImportMessages.CSVImport_imports_dialog_error, null);
        		for (int i = 0; i < selectedImports.size(); i++) {
        			String selectedImport = selectedImports.get(i);
    				imports.remove(selectedImport);
        		}
        		if (!result.isOK()) {
        			StatusUtil.handleStatus(result, StatusManager.SHOW);
        		}
        		importsViewer.refresh(false);
        	}
        });
		}

    /**
     *	Create the group for creating the source CSV file input area
     */
    protected void createTemplateFileGroup(Composite parent) {
        Composite templateFileGroup = new Composite(parent, SWT.NONE);
		GridLayout fileLayout = new GridLayout();
		fileLayout = new GridLayout();
		fileLayout.numColumns = 3;
		fileLayout.marginHeight = 0;
		fileLayout.marginWidth = 0;
		fileLayout.makeColumnsEqualWidth = false;
		templateFileGroup.setLayout(fileLayout);
		GridData gridData = GridDataUtil.createHorizontalFill();
		templateFileGroup.setLayoutData(gridData);

        Label groupLabel = new Label(templateFileGroup, SWT.NONE);
        groupLabel.setText(SADLImportMessages.CSVImport_template_title);
        groupLabel.setFont(parent.getFont());

		templateFileText = new Text(templateFileGroup, SWT.BORDER); 
		gridData = GridDataUtil.createHorizontalFill();
		gridData.widthHint = 300;
		templateFileText.setLayoutData(gridData);
		templateFileText.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });
		templateFileText.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent e) {
                //Do nothing when getting focus
            }
            public void focusLost(FocusEvent e) {
            	String f = templateFileText.getText();
                if (f == null || f.isEmpty()) {
                	// Set enter template file message
                	setMessage(SADLImportMessages.CSVImport_template_field_message);
                } else if (validFileName(f)) {
                	List<String> templateImports = scanTemplateForImports(f);
                	if (templateImports.size() > 0) {
                		imports.clear();
                		for (int i=0; i<templateImports.size(); i++) {
                			imports.add(templateImports.get(i));
                		}
                	}
                	if (e.getSource().equals(namespaceText)) {
                		// this is an explicit change to the namespace; overrides the default
                		namespaceText.setText(uri);
                		namespaceExplicitlySet = true;
                	}
                	importsViewer.refresh();
                	// Clear enter template file message
                	setErrorMessage(null);
                	setMessage(null);
                } else {
                	// Display error message
                	setErrorMessage(SADLImportMessages.CSVImport_invalid_file_message);
                }
            }
        });

        // template browse button
		templateBrowseButton = new Button(templateFileGroup, SWT.PUSH);
		templateBrowseButton.setText(SADLImportMessages.DataTransfer_browse);
		templateBrowseButton.addListener(SWT.Selection, this);
		templateBrowseButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));
		templateBrowseButton.setFont(parent.getFont());
        setButtonLayoutData(templateBrowseButton);

        // first row contains headings button
        Composite optionComposite = new Composite(templateFileGroup, SWT.NONE);        
		GridLayout optionLayout = new GridLayout();
		optionLayout = new GridLayout();
		optionLayout.numColumns = 2;
		optionLayout.marginHeight = 0;
		optionLayout.marginWidth = 0;
		optionLayout.makeColumnsEqualWidth = false;
		optionComposite.setLayout(optionLayout);
		gridData = GridDataUtil.createHorizontalFill();
		gridData.horizontalSpan = 3;
		optionComposite.setLayoutData(gridData);		
    }

    /**
     *	Create the import source specification widgets
     */
    protected void createSourceGroup(Composite parent) {
    	createRestoreGroup(parent);
    	createSourceCSVGroup(parent);
    	createNamespaceGroup(parent);
    	createTemplateFileGroup(parent);
    	createImportsGroup(parent);
    }

    /**
     *	Create the restore previous values checkbox
     */
    protected void createRestoreGroup(Composite parent) {
        Composite optionComposite = new Composite(parent, SWT.NONE);        
		GridLayout optionLayout = new GridLayout();
		optionLayout = new GridLayout();
		optionLayout.numColumns = 2;
		optionLayout.marginHeight = 0;
		optionLayout.marginWidth = 0;
		optionLayout.makeColumnsEqualWidth = false;
		optionComposite.setLayout(optionLayout);
		GridData gridData = GridDataUtil.createHorizontalFill();
		gridData.horizontalSpan = 3;
		optionComposite.setLayoutData(gridData);
		
		restoreCheckbox = new Button(optionComposite, SWT.CHECK);
		restoreCheckbox.setSelection(true);
		restoreCheckbox.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		if (restoreCheckbox.getSelection()) {
        			restoreWidgetValues();
        		} else {
        			clearWidgetValues();
        		}
        	}
        });
		Label restoreLabel = new Label(optionComposite, SWT.NULL);
		restoreLabel.setText(SADLImportMessages.CSVImport_restore_label);
    }

    /**
     * Enable or disable the button group.
     */
    protected void enableButtonGroup(boolean enable) {
        selectTypesButton.setEnabled(enable);
        selectAllButton.setEnabled(enable);
        deselectAllButton.setEnabled(enable);
    }

    /**
     *	Answer a boolean indicating whether the specified source currently exists
     *	and is valid
     */
    protected boolean ensureSourceIsValid() {
        if (new File(getSourceFileName()) != null) {
			return true;
		}
        setErrorMessage(SADLImportMessages.FileImport_invalidSource);
        return false;
    }

    /**
     *	Execute the passed import operation.  Answer a boolean indicating success.
     */
    protected boolean executeCsvImportOperation(CsvImportOperation op) {
        initializeOperation(op);

        try {
            getContainer().run(true, true, op);
        } catch (InterruptedException e) {
            return false;
        } catch (InvocationTargetException e) {
            displayErrorDialog(e.getTargetException());
            return false;
        }

        IStatus status = op.getStatus();
        if (!status.isOK()) {
            ErrorDialog
                    .openError(getContainer().getShell(), SADLImportMessages.FileImport_importProblems,
                            null, // no special message
                            status);
            return false;
        }

        return true;
    }

    /**
     *	The Finish button was pressed.  Try to do the required work now and answer
     *	a boolean indicating success.  If false is returned then the wizard will
     *	not close.
     *
     * @return boolean
     */
    public boolean finish() {
        if (!ensureSourceIsValid()) {
			return false;
		}
    	String f = namespaceText.getText();
        if (f == null || f.isEmpty()) {
        	// Set enter namespace message (namespace is optional for now)
        	setMessage(SADLImportMessages.CSVImport_namespace_field_message);
        	return false;
        } else if (!validURI(f)) {
        	// Display error message
        	setErrorMessage(SADLImportMessages.CSVImport_invalid_namespace_message);
        	return false;
        }
        else {
        	uri = f;
        }
    	f = templateFileText.getText();
        if (f == null || f.isEmpty()) {
        	// Set enter template file message
        	setMessage(SADLImportMessages.CSVImport_template_field_message);
        	return false;
        } else if (!validFileName(f)) {
        	// Display error message
        	setErrorMessage(SADLImportMessages.CSVImport_invalid_file_message);
        	return false;
        }

        saveWidgetValues();

		return importResources();
    }


    /**
     *	Answer the CSV file specified as being the import source.
     */
    private String getSourceFileName() {
        return this.csvFileText.getText();
    }

    /**
     *	Answer the string to display as the label for the source specification field
     */
    protected String getSourceLabel() {
        return SADLImportMessages.FileImport_fromDirectory;
    }

    /**
     *	Handle all events and enablements for widgets in this dialog
     *
     * @param event Event
     */
    public void handleEvent(Event event) {
        if (event.widget == sourceBrowseButton) {
			handleSourceBrowseButtonPressed();
        } else if (event.widget == templateBrowseButton) {
			handleTemplateBrowseButtonPressed();
		}
        super.handleEvent(event);
    }

    /**
     *	Open an appropriate file browser so that the user can specify a source
     *	to import from
     */
    protected void handleSourceBrowseButtonPressed() {

        String currentSource = this.csvFileText.getText();
        FileDialog dialog = new FileDialog(
        		csvFileText.getShell(), SWT.SAVE | SWT.SHEET);
        dialog.setText(SELECT_SOURCE_TITLE);
//        dialog.setFilterPath(getSourceDirectoryName(currentSource));
        if (currentSource != null) {
        	dialog.setFilterPath((new File(currentSource)).getParent());
        }
        String selectedFile = dialog.open();
        if (selectedFile != null) {
            if (validFileName(selectedFile)) {
            	// Clear enter csv file message
            	setErrorMessage(null);
            	setMessage(null);
            	// Populate namespace if necessary
            	String ns = namespaceText.getText();
            	if (ns == null || ns.isEmpty()) {
                	prepopulateNamespace(selectedFile);
            	}
            } else {
            	// Display error message
            	setErrorMessage(SADLImportMessages.CSVImport_invalid_file_message);
            }
            setErrorMessage(null);
            csvFileText.setText(selectedFile);
            csvFileText.setFocus();
            addButton.setEnabled(true);
        } else {
        	setMessage(SADLImportMessages.CSVImport_csv_field_message);
        }
    }

    /**
     *	Open an appropriate file browser so that the user can specify a template
     *	file to use
     */
    protected void handleTemplateBrowseButtonPressed() {

        String currentTemplate = this.templateFileText.getText();
        FileDialog dialog = new FileDialog(
        		templateFileText.getShell(), SWT.SAVE | SWT.SHEET);
        dialog.setText(SADLImportMessages.CSVImport_template_dialog_title);
//        dialog.setFilterPath(currentTemplate);
        if (currentTemplate != null) {
        	dialog.setFilterPath((new File(currentTemplate)).getParent());
        }
        String selectedFile = dialog.open();
        if (selectedFile != null) {
            if (validFileName(selectedFile)) {
            	List<String> templateImports = scanTemplateForImports(selectedFile);
            	if (templateImports.size() > 0) {
            		imports.clear();
            		for (int i=0; i<templateImports.size(); i++) {
            			imports.add(templateImports.get(i));
            		}
            	}
            	if (!namespaceExplicitlySet) {
            		namespaceText.setText(uri);
            	}
            	importsViewer.refresh();
            	// Clear enter template file message
            	setErrorMessage(null);
            	setMessage(null);
            } else {
            	// Display error message
            	setErrorMessage(SADLImportMessages.CSVImport_invalid_file_message);
            }
            setErrorMessage(null);
            templateFileText.setText(selectedFile);
            templateFileText.setFocus();
        } else {
        	setMessage(SADLImportMessages.CSVImport_template_field_message);
        }
    }

    /**
     *  Import the resources with extensions as specified by the user
     */
    protected boolean importResources() {
    	File csvFile = new File(this.csvFileText.getText());
    	File csvDirectory = csvFile.getParentFile();
//    	File csvFileName = new File(csvFile.getName());
    	if (template == null) {
    		String templateFN = this.templateFileText.getText();
    		// set template
    		scanTemplateForImports(templateFN);
    	}
    	List<Object> csvFilesList = new ArrayList<Object>();
//    	csvFilesList.add(csvFileName);
    	csvFilesList.add(csvFile);
    	boolean headers = this.firstRowVarButton.getSelection();
    	boolean debugOutput = this.debugOutputButton.getSelection();
    	String namespace = this.namespaceText.getText();
//    	String template = this.templateFileText.getText();
    	String destFileType = ".owl";
    	if (this.importAsSadlCheckbox.getSelection()) {
    		destFileType = ".sadl";
    	}
        CsvImportOperation operation = new CsvImportOperation(getContainerFullPath(),
        		csvDirectory, fileSystemStructureProvider, this, csvFilesList,
        		headers, debugOutput, namespace, this.imports, this.template, destFileType);        
        operation.setContext(getShell());
        return executeCsvImportOperation(operation);
    }

    protected void handleContainerBrowseButtonPressed() {
    	super.handleContainerBrowseButtonPressed();
    	IPath path = getContainerFullPath();
    	if (path != null && relativePathVariableGroup != null) {
			IResource target = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
//			File file = getSourceDirectory();
			File file = null;
			if (file != null && target != null) {
				relativePathVariableGroup.setupVariableContent();
				String preferedVariable = RelativePathVariableGroup.getPreferredVariable(new IPath[] {Path.fromOSString(file.getAbsolutePath())}, (IContainer) target);
				if (preferedVariable != null)
					relativePathVariableGroup.selectVariable(preferedVariable);
			}
    	}
    	updateWidgetEnablements();
    	if (this.namespaceText.getText().contains("/${project_name}")) {
    		IPath projectPath = path.removeLastSegments(path.segmentCount()-1);
    		String newNamespace = this.namespaceText.getText().replaceFirst("/\\$\\{project_name\\}",
    				projectPath.toString());
    		this.namespaceText.setText(newNamespace);
    	}
    }

   	/**
     * Initializes the specified operation appropriately.
     */
    protected void initializeOperation(CsvImportOperation op) {
        op.setCreateContainerStructure(false);
        op.setOverwriteResources(overwriteExistingResourcesCheckbox
                .getSelection());
    }

   	/**
     * Clears widget values when restore checkbox is toggled
     */
    protected void clearWidgetValues() {	
		csvFileText.setText(getDefaultCsvFilename());
		firstRowVarButton.setSelection(true);
		debugOutputButton.setSelection(false);
		namespaceText.setText("");
		templateFileText.setText("");
		imports.clear();
		importsViewer.setInput(imports);
		setContainerFieldValue(getDefaultOutputFolder());
		overwriteExistingResourcesCheckbox.setSelection(false);
        createTopLevelFolderCheckbox.setSelection(false);
        importAsOwlCheckbox.setSelection(true);
        importAsSadlCheckbox.setSelection(false);
    	updateWidgetEnablements();
    }
    
    private String getDefaultOutputFolder() {
    	IWizard wizard = getWizard();
     	String outputFolder = "";
    	if (wizard instanceof SadlCSVImportWizard) {
    		IStructuredSelection selection = ((SadlCSVImportWizard)wizard).getSelection();
    		Object element = selection.getFirstElement();
    		if (element instanceof IFile)  {
    			outputFolder = ((IFile)element).getProject().getName();
      		}
    	}
    	return outputFolder;
    }
    
    private String getDefaultCsvFilename() {
    	IWizard wizard = getWizard();
    	String selectedCsvFile = "";
     	if (wizard instanceof SadlCSVImportWizard) {
    		IStructuredSelection selection = ((SadlCSVImportWizard)wizard).getSelection();
    		Object element = selection.getFirstElement();
    		if (element instanceof IFile)  {
     			if (((IFile)element).getFileExtension().equals("csv")) {
    				selectedCsvFile = ((IFile)element).getRawLocationURI().getPath();
    			}
     		}
    	}
     	return selectedCsvFile;
    }

    /**
     *	Use the dialog store to restore widget values to the values that they held
     *	last time this wizard was used to completion
     */
    protected void restoreWidgetValues() {
        IDialogSettings settings = getDialogSettings();
        if (settings != null) {
        	boolean b;
        	String csvPath = settings.get(STORE_SOURCE_CSV_FILE_ID);
        	if (csvPath == null) csvPath = "";
        	csvFileText.setText(csvPath);
        	b = settings.getBoolean(STORE_HEADER_ROW_ID);
        	firstRowVarButton.setSelection(b);
        	String ns = settings.get(STORE_IMPORT_MODEL_NAMESPACE_ID);
        	if (ns == null) ns = "";
        	namespaceText.setText(ns);
        	String templatePath = settings.get(STORE_TEMPLATE_FILE_ID);
        	if (templatePath == null) templatePath = "";
        	templateFileText.setText(templatePath);
    		imports.clear();
            String[] importsArray = settings.getArray(STORE_IMPORTS_ID);
            if (importsArray != null) {
				for (int i=0; i<importsArray.length; i++) {
					imports.add(importsArray[i]);
				}
			}
    		importsViewer.setInput(imports);
            String destFolder = settings.get(STORE_DESTINATION_FOLDER_ID);
        	if (destFolder == null || destFolder.trim().length() < 1) destFolder = getDefaultOutputFolder();
            setContainerFieldValue(destFolder);
            // radio buttons and checkboxes
            b = settings.getBoolean(STORE_OVERWRITE_EXISTING_RESOURCES_ID);
            overwriteExistingResourcesCheckbox.setSelection(b);
            b = settings.getBoolean(STORE_CREATE_CONTAINER_STRUCTURE_ID);
            createTopLevelFolderCheckbox.setSelection(b);
            b = settings.getBoolean(STORE_IMPORT_AS_OWL_ID);
            importAsOwlCheckbox.setSelection(b);
            b = settings.getBoolean(STORE_IMPORT_AS_SADL_ID);
            importAsSadlCheckbox.setSelection(b);
        	updateWidgetEnablements();
        }
        if (csvFileText.getText().trim().length() < 1) {
        	csvFileText.setText(getDefaultCsvFilename());
        }
     }

    /**
     * 	Since Finish was pressed, write widget values to the dialog store so that they
     *	will persist into the next invocation of this wizard page
     */
    protected void saveWidgetValues() {
        IDialogSettings settings = getDialogSettings();
        if (settings != null) {
        	settings.put(STORE_SOURCE_CSV_FILE_ID,
        			csvFileText.getText());
        	settings.put(STORE_HEADER_ROW_ID,
        			firstRowVarButton.getSelection());
        	settings.put(STORE_IMPORT_MODEL_NAMESPACE_ID,
        			namespaceText.getText());
            settings.put(STORE_TEMPLATE_FILE_ID,
            		templateFileText.getText());
        	settings.put(STORE_IMPORTS_ID, imports.toArray(new String[imports.size()]));
        	String destFolder = getContainerFullPath().makeRelative().toString();
        	settings.put(STORE_DESTINATION_FOLDER_ID, destFolder);
            // radio buttons and checkboxes
            settings.put(STORE_OVERWRITE_EXISTING_RESOURCES_ID,
                    overwriteExistingResourcesCheckbox.getSelection());
            settings.put(STORE_CREATE_CONTAINER_STRUCTURE_ID,
                    createTopLevelFolderCheckbox.getSelection());
            settings.put(STORE_IMPORT_AS_OWL_ID,
            		importAsOwlCheckbox.getSelection());
            settings.put(STORE_IMPORT_AS_SADL_ID,
            		importAsSadlCheckbox.getSelection());
        }
    }


    /* (non-Javadoc)
     * Method declared on IDialogPage. Set the selection up when it becomes visible.
     */
    public void setVisible(boolean visible) {
        super.setVisible(visible);
//        resetSelection();
        if (visible) {
//        	this.selectionGroup.setFocus();
//			this.sourceNameField.setFocus();
			this.csvFileText.setFocus();
		}
    }

    /**
     * Check if widgets are enabled or disabled by a change in the dialog.
     * Provided here to give access to inner classes.
     */
    protected void updateWidgetEnablements() {
        super.updateWidgetEnablements();
//        enableButtonGroup(ensureSourceIsValid());
    }

    /**
     *	Answer a boolean indicating whether self's source specification
     *	widgets currently all contain valid values.
     */
    protected boolean validateSourceGroup() {
    	try {
    		File sourceFile = new File(csvFileText.getText());
    	} catch (Exception e) {
            setMessage(SADLImportMessages.CSVImport_csv_field_message);
            enableButtonGroup(false);
            return false;
        }

//		enableButtonGroup(true);
		setErrorMessage(null);
        return true;
    }

    private boolean validFileName(String fileName) {
    	File f = new File(fileName);
    	if (f.isFile() && f.exists() && f.canRead()) {
    		return true;
    	}
    	return false;
    }
    
    private boolean validURI(String s) {
    	try {
    		URI uri = new URI(s);
    	} catch (Exception e) {
    		return false;
    	}
    	return true;
    }

    /**
     * Returns a content provider for <code>FileSystemElement</code>s that returns
     * only files as children.
     */
    protected ITreeContentProvider getFileProvider() {
        return new WorkbenchContentProvider() {
            public Object[] getChildren(Object o) {
                if (o instanceof MinimizedFileSystemElement) {
                    MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
                    return element.getFiles(
                    		fileSystemStructureProvider).getChildren(
                            element);
                }
                return new Object[0];
            }
        };
    }

    /**
     * Returns a content provider for <code>FileSystemElement</code>s that returns
     * only folders as children.
     */
    protected ITreeContentProvider getFolderProvider() {
        return new WorkbenchContentProvider() {
            public Object[] getChildren(Object o) {
                if (o instanceof MinimizedFileSystemElement) {
                    MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
                    return element.getFolders(
                    		fileSystemStructureProvider).getChildren(
                            element);
                }
                return new Object[0];
            }

            public boolean hasChildren(Object o) {
                if (o instanceof MinimizedFileSystemElement) {
                    MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
                    if (element.isPopulated()) {
						return getChildren(element).length > 0;
					}

                    //If we have not populated then wait until asked
                    return true;
                }
                return false;
            }
        };
    }

    private List<String> scanTemplateForImports(String fileName) {
    	StringBuffer templateSb = new StringBuffer();
    	boolean usesCR = false;
    	File f = new File(fileName);
    	List<String> templateImports = new ArrayList<String>();
		try {
			Scanner s = new Scanner(f);
			if (s != null) {
				s.useDelimiter("\\n");
				while (s.hasNext()) {
					String templateLine = s.next();
					templateLine = CsvImporter.dropEOS(templateLine);
					if (templateLine.endsWith("\r")) {
						templateLine = templateLine.substring(0,templateLine.length()-1);
						usesCR = true;
					}
					templateLine = templateLine.trim();
					if (templateLine.matches("\\s*import\\s+\\S*")) {
						String imp = templateLine.replaceFirst("\\s*import\\s+","");
						if (imp.startsWith("\"") && imp.endsWith("\"")) {
							imp = imp.substring(1, imp.length() - 1);
						}
						templateImports.add(imp);
					}
					else if (templateLine.matches("\\s*uri\\s+(\\S|\\s)*")) {
						String uri = templateLine.replaceFirst("\\s*uri\\s+","");
						int lastQuote = uri.lastIndexOf("\"");
						if (lastQuote > 0) {
							uri = uri.substring(0, lastQuote + 1);
						}
						if (uri.startsWith("\"") && uri.endsWith("\"")) {
							uri = uri.substring(1, uri.length() - 1);
						}
						this.uri = uri;
						namespaceText.setText(uri);
					} 
	//				else {
						templateSb.append(templateLine);
						if (usesCR) {
							templateSb.append("\r\n");
						} else {
							templateSb.append("\n");
						}
	//				}
				}
				s.close();
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		this.template = templateSb.toString();
		return templateImports;
    }

    private void prepopulateNamespace(String f) {
    	IPreferencesService ps = Platform.getPreferencesService();
    	String baseUri = ps.getString("com.ge.research.sadl.Sadl", "baseUri", "http://sadl.org/${project_name}/", null);
		if (!baseUri.endsWith("/")) {
			baseUri += "/";
		}
		org.eclipse.emf.common.util.URI csvURI = org.eclipse.emf.common.util.URI.createFileURI(f);
		String uri = baseUri + csvURI.trimFileExtension().lastSegment();
        namespaceText.setText(uri);
    }
}
