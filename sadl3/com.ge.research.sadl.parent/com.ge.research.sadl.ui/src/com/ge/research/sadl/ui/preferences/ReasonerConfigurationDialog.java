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
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/22 20:32:53 $
 ***********************************************************************/

package com.ge.research.sadl.ui.preferences;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogMessageArea;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.reasoner.ConfigurationOption;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.ui.widgets.DoubleFieldEditor;

public class ReasonerConfigurationDialog extends Dialog {
	
	private static final Logger logger = LoggerFactory.getLogger(ReasonerConfigurationDialog.class);
	
	private IConfigurationManagerForIDE configurationManager;
	
	private String reasonerCategory;
	private Map<String, ConfigurationOption> config;
	private Map<String, Object> currentConfig;
	private int nOptions;
	private int width;

    private FieldEditorPreferencePage page;
	IPreferenceStore rcps;
    private List<FieldEditor> editors;
    private DialogMessageArea messageArea;

    protected ReasonerConfigurationDialog(Shell parentShell) {
		super(parentShell);
		// TODO Auto-generated constructor stub
	}

	protected ReasonerConfigurationDialog(Shell parentShell, String reasonerCategory, 
			                              Map<String, ConfigurationOption> config,
			                              IConfigurationManagerForIDE _configurationManager) {
		super(parentShell);
		this.reasonerCategory = reasonerCategory;
		this.config = config;
		configurationManager = _configurationManager;		
		processConfigurationOptions();
		currentConfig = new HashMap<String, Object>();
		String[] catHier = new String[1];
		catHier[0] = this.reasonerCategory;
		try {
			List<ConfigurationItem> cis = configurationManager.getConfiguration(catHier, true);
			if (cis != null && cis.size() > 0) {
				ConfigurationItem ci = cis.get(0);
				List<ConfigurationItem.NameValuePair> nvPairs = ci.getNameValuePairs();
				for ( ConfigurationItem.NameValuePair nv : nvPairs) {
					currentConfig.put(nv.getName(), nv.getValue());
				}
			}
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(reasonerCategory+" Configuration Options");
        this.initializeDialogUnits(newShell);
	    width = this.convertWidthInCharsToPixels(width);
	    int height = this.convertHeightInCharsToPixels(14+2*nOptions);
        newShell.setSize(width, height);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        
    	editors = new ArrayList<FieldEditor>();
        messageArea = new DialogMessageArea();

        page = new FieldEditorPreferencePage(FieldEditorPreferencePage.GRID) {
            @Override
			public void createControl(Composite parentComposite) {
				noDefaultAndApplyButton();
				super.createControl(parentComposite);
			}
            
            @SuppressWarnings("synthetic-access")
			@Override
		    protected void createFieldEditors() {
            	rcps = this.doGetPreferenceStore();
            	if (rcps == null) {
            		rcps = new PreferenceStore();
            	}
            	FieldEditor editor;
            	if (config != null) {
	            	for (Map.Entry<String, ConfigurationOption> entry : config.entrySet()) {
	        		    String key = entry.getKey();
	        		    ConfigurationOption option = entry.getValue();
	        		    if (key.equalsIgnoreCase("builtin")) {
	        		    	continue;
	        		    }
	        		    String optionDescription = option.getDescription();
	        		    Object currentValue = currentConfig.get(key);
	        		    Object optionValue = option.getValue();
	        		    if (currentValue != null) {
	        		    	optionValue = currentValue;
	        		    }
	        		    logger.debug(key+" class = "+optionValue.getClass().getName());
	        		    Object[] optionPossibleValues = option.getPossibleValues();
	        		    if (optionPossibleValues != null && optionPossibleValues.length > 0) {
	        		    	// Option has a list of values so create a dropdown box
	        		    	String[][] nv = new String[optionPossibleValues.length][2];
	        		    	for (int i=0; i<optionPossibleValues.length; i++) {
	        		    		nv[i][0] = optionPossibleValues[i].toString();
	        		    		nv[i][1] = optionPossibleValues[i].toString();
	        		    	}
	        		    	editor = new ComboFieldEditor(key, optionDescription, nv, getFieldEditorParent());
	        		    	rcps.setValue(editor.getPreferenceName(), optionValue.toString());
	        		    	editor.setPreferenceStore(rcps);
	        		    	addField(editor);
	        		    	editor.load();
	        		    	editors.add(editor);
	        		    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.String")) {
	        		    	editor = new StringFieldEditor(key, optionDescription, getFieldEditorParent());
	        		    	rcps.setValue(editor.getPreferenceName(), optionValue.toString());
	        		    	editor.setPreferenceStore(rcps);
	        		    	addField(editor);
	        		    	editor.load();        		    	
	        		    	editors.add(editor);
	        		    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.Boolean")) {
	        		    	editor = new BooleanFieldEditor(key, optionDescription, BooleanFieldEditor.SEPARATE_LABEL, getFieldEditorParent());
	        		    	rcps.setValue(editor.getPreferenceName(), optionValue.toString());
	        		    	editor.setPreferenceStore(rcps);
	        		    	addField(editor);
	        		    	editor.load();        		    	
	        		    	editors.add(editor);
	        		    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.Integer")) {
	        		    	editor = new IntegerFieldEditor(key, optionDescription, getFieldEditorParent());
	        		    	rcps.setValue(editor.getPreferenceName(), optionValue.toString());
	        		    	editor.setPreferenceStore(rcps);
	        		    	addField(editor);
	        		    	editor.setPage(page);
	        		    	editor.load();        		    	
	        		    	editors.add(editor);
	        		    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.Double")) {
	        		    	editor = new DoubleFieldEditor(key, optionDescription, getFieldEditorParent());
	        		    	rcps.setValue(editor.getPreferenceName(), optionValue.toString());
	        		    	editor.setPreferenceStore(rcps);
	        		    	addField(editor);
	        		    	editor.load();        		    	
	        		    	editors.add(editor);
	        		    }
	            	}
            	}
            	else {
            		logger.info("No configuration options available");
            	}
            }
            
			@SuppressWarnings("synthetic-access")
			@Override
			protected void updateApplyButton() {
				updateButtons(isValid());
				super.updateApplyButton();
			}
			
        };

        messageArea.createContents(composite);
        messageArea.showTitle(reasonerCategory+" Configuration Options",null);
        messageArea.setMessageLayoutData(new GridData(GridData.FILL_BOTH));
        page.createControl(composite);
		for (FieldEditor editor : editors) {
			editor.setPreferenceStore(rcps);
		}
        Control pageControl = page.getControl();
        pageControl.setLayoutData(new GridData(GridData.FILL_BOTH));
        return pageControl;
    }

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);
		updateButtons(page.isValid());
	}

	private void updateButtons(boolean isValid) {
		Button okButton = getButton(IDialogConstants.OK_ID);
		if (okButton != null) {
			okButton.setEnabled(isValid);
		}
		if (isValid) {
			messageArea.updateText(null, IMessageProvider.NONE);
		} else {
			messageArea.updateText(getErrorMessages(), IMessageProvider.ERROR);
		}
	}

	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// make each editor save its value
			for (FieldEditor editor : editors) {
				editor.store();
			}
			// copy updated values into configuration map
			updateConfigurationOptions();
		}
		super.buttonPressed(buttonId);
	}


	private void processConfigurationOptions() {
		nOptions = 0;
		int maxDescL = -1;
		int maxOptsL = 20;
		if (config != null) {
			for (Map.Entry<String, ConfigurationOption> entry : config.entrySet()) {
			    String key = entry.getKey();
			    ConfigurationOption option = entry.getValue();
			    if (key.equalsIgnoreCase("builtin")) {
			    	continue;
			    }
			    nOptions++;
			    // String optionName = option.getName();
			    String optionDescription = option.getDescription();
			    if (optionDescription.length() > maxDescL) {
			    	maxDescL = optionDescription.length();
			    }
			    // String[] optionHierarchy = option.getCategoryHierarchy();
			    Object optionValue = option.getValue();
			    if (optionValue.toString().length() > maxOptsL) {
			    	maxOptsL = optionValue.toString().length();
			    }
			    Object[] optionPossibleValues = option.getPossibleValues();
			    if (optionPossibleValues != null && optionPossibleValues.length > 0) {
			    	for (int i=0; i<optionPossibleValues.length; i++) {
			    		if (optionPossibleValues[i].toString().length() > maxOptsL) {
			    			maxOptsL = optionPossibleValues[i].toString().length();
			    		}
			    	}
			    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.String")) {
			    	int size = Math.max(optionValue.toString().length(), 40);
			    	if (size > maxOptsL) {
			    		maxOptsL = size;
			    	}
			    }
			    // logger.debug("maxDescL = "+maxDescL+"   maxOptsL = "+maxOptsL);
			}
		}
		else {
			logger.info("No reasoner configurations available.");
		}
	    width = maxDescL + 5 + maxOptsL + 15;
	    logger.debug("maxDescL = "+maxDescL+"   maxOptsL = "+maxOptsL+"   width = "+width);
	}
	
	private void updateConfigurationOptions() {
	    String[] categoryHierarchy = new String[1];
	    categoryHierarchy[0] = this.reasonerCategory;
	    List<ConfigurationItem> configItems = null;
		try {
			configItems = configurationManager.getConfiguration(categoryHierarchy, true);
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		logger.debug("Size of configItem = " + (configItems != null ? configItems.size() : 0));
		
		ConfigurationItem configItem;
		if (configItems != null) {
			configItem = configItems.get(0);
		}
		else {
			configItem = new ConfigurationItem(categoryHierarchy);
		}
		configItem.clearNameValuePairs();
		for (Map.Entry<String, ConfigurationOption> entry : config.entrySet()) {
		    String key = entry.getKey();
		    ConfigurationOption option = entry.getValue();
		    if (key.equalsIgnoreCase("builtin")) {
		    	continue;
		    }
		    String optionName = option.getName();
		    Object optionValue = option.getValue();
		    if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.String")) {
		    	optionValue = this.rcps.getString(key);
		    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.Double")) {
		    	optionValue = this.rcps.getDouble(key);
		    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.Integer")) {
		    	optionValue = this.rcps.getInt(key);
		    } else if (optionValue.getClass().getName().equalsIgnoreCase("java.lang.Boolean")) {
		    	optionValue = this.rcps.getBoolean(key);
		    } else {
		    	logger.debug("Error saving configuration option, "+optionName+", of type "+optionValue.getClass().getName());
		    }
		    logger.debug("Setting "+optionName+" to "+optionValue.toString());
		    ConfigurationItem.NameValuePair nv = configItem.new NameValuePair(optionName, optionValue);
		    nv.setConfigType(ConfigurationItem.ConfigurationType.SingleValue);
		    configItem.addNameValuePair(nv);
		}
		try {
			configurationManager.updateConfiguration(configItem);
			configurationManager.saveConfiguration();
		} catch (ConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private String getErrorMessages() {
		String messages = new String();
		for (FieldEditor editor : editors) {
			if (!editor.isValid()) {
				String message = ((StringFieldEditor) editor).getErrorMessage();
				if (messages.length() > 0) {
					messages += "; ";
				}
				messages += message;
			}
		}
		return messages;
	}
}
