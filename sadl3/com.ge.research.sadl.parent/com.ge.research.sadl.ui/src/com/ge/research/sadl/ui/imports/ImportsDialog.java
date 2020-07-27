package com.ge.research.sadl.ui.imports;

import java.net.URI;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.layout.LayoutConstants;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.WorkbenchPlugin;

/**
 * This class is used to prompt the user for a owl/rdf import.
 */
public class ImportsDialog extends TitleAreaDialog {

    private static final String DIALOG_SETTINGS_SECTION = "SADL_ImportsDialogSettings"; //$NON-NLS-1$
    private static final String SADL_CSVIMPORT_IMPORTS_DIALOG = "SADL_CSVIMPORT_IMPORTS_DIALOG";
    private String importValue = ""; //$NON-NLS-1$
    private String initialValue;
    private Text importField;
    private Button okButton;
    private String title;
    private String helpContextId;
    private final String headerTitle;
    private final String message;
    private final String label;

    /**
     * Constructs a new owl/rdf import dialog.
     * @param parentShell the parent shell
     */
    public ImportsDialog(Shell parentShell) {
        this (parentShell, SADLImportMessages.CSVImport_imports_dialog_shellTitle,
        		SADL_CSVIMPORT_IMPORTS_DIALOG,
        		SADLImportMessages.CSVImport_imports_dialog_dialogTitle,
        		SADLImportMessages.CSVImport_imports_dialog_message,
        		SADLImportMessages.CSVImport_imports_dialog_label);
        setShellStyle(getShellStyle() | SWT.SHEET);
    }

    /**
     * Constructs a new file extension dialog.
     * 
     * @param parentShell the parent shell
     * @param title the dialog title
     * @param helpContextId the help context for this dialog
     * @param headerTitle the dialog header
     * @param message the dialog message
     * @param label the label for the "file type" field
     * @since 3.4
     */
    public ImportsDialog(Shell parentShell, String title, String helpContextId, 
    		String headerTitle, String message, String label) {
        super(parentShell);
        this.title = title;
        this.helpContextId = helpContextId;
        this.headerTitle = headerTitle;
        this.message = message;
        this.label = label;
        setShellStyle(getShellStyle() | SWT.SHEET);
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
     */
    protected void configureShell(Shell shell) {
        super .configureShell(shell);
        shell.setText(title);
        PlatformUI.getWorkbench().getHelpSystem().setHelp(shell,
                helpContextId);
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    protected Control createDialogArea(Composite parent) {
        Composite parentComposite = (Composite) super.createDialogArea(parent);
        Composite contents = new Composite(parentComposite, SWT.NONE);
        contents.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        setTitle(headerTitle);
        setMessage(this.message);
        new Label(contents, SWT.LEFT).setText(label);
        importField = new Text(contents, SWT.SINGLE | SWT.BORDER);
        if (initialValue != null) {
        	importField.setText(initialValue);
        }
        importField.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (event.widget == importField) {
                	importValue = importField.getText().trim();
                	if (!importValue.isEmpty()) {
                		okButton.setEnabled(true);
                	}
                }
            }
        });
        importField.setFocus();
        Dialog.applyDialogFont(parentComposite);
        Point defaultMargins = LayoutConstants.getMargins();
        GridLayoutFactory.fillDefaults().numColumns(2).margins(
                defaultMargins.x, defaultMargins.y).generateLayout(contents);
        return contents;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
     */
    protected void createButtonsForButtonBar(Composite parent) {
        okButton = createButton(parent, IDialogConstants.OK_ID,
                IDialogConstants.OK_LABEL, true);
        okButton.setEnabled(false);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

   	protected void okPressed() {
   		if (!validURI(importValue)) {
   			// Display error message
           	setErrorMessage(SADLImportMessages.CSVImport_invalid_imports_message);
   			this.setReturnCode(Window.CANCEL);
   		} else {
   			setErrorMessage(null);
   			this.setReturnCode(Window.OK);
   			super.okPressed();
   		}
   	}

    /**
     * Sets the initial value that should be pre-populated in this dialog.
     * 
     * @param initialValue the value to be displayed to the user
     */
    public void setInitialValue(String initialValue) {
        this.initialValue = initialValue;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#getDialogBoundsSettings()
     */
    protected IDialogSettings getDialogBoundsSettings() {
        IDialogSettings settings = WorkbenchPlugin.getDefault()
                .getDialogSettings();
        IDialogSettings section = settings
                .getSection(DIALOG_SETTINGS_SECTION);
        if (section == null)
            section = settings.addNewSection(DIALOG_SETTINGS_SECTION);
        return section;
    }

    /*
     * (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#isResizable()
     */
    protected boolean isResizable() {
        return true;
    }

    /* Gets the importValue
     * @return importValue
     */
    public String getImportValue() {
        return this.importValue;
    }
    
    private boolean validURI(String s) {
    	try {
    		URI uri = new URI(s);
    	} catch (Exception e) {
    		return false;
    	}
    	return true;
    }

}