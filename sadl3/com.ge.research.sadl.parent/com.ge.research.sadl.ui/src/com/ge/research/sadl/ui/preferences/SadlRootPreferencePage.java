/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.preferences;

import static com.ge.research.sadl.preferences.SadlPreferences.CHECK_FOR_AMBIGUOUS_NAMES;
import static com.ge.research.sadl.preferences.SadlPreferences.CHECK_FOR_CARDINALITY_OF_PROPERTY_IN_DOMAIN;
import static com.ge.research.sadl.preferences.SadlPreferences.CONTENT_ASSIST__FILTER_IMPLICIT_MODEL;
import static com.ge.research.sadl.preferences.SadlPreferences.CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES;
import static com.ge.research.sadl.preferences.SadlPreferences.DEEP_VALIDATION_OFF;
import static com.ge.research.sadl.preferences.SadlPreferences.DMY_ORDER_DMY;
import static com.ge.research.sadl.preferences.SadlPreferences.DMY_ORDER_MDY;
import static com.ge.research.sadl.preferences.SadlPreferences.FIND_AND_EXPAND_MISSING_PATTERNS;
import static com.ge.research.sadl.preferences.SadlPreferences.GENERATE_METRICS_REPORT_ON_CLEAN_BUILD;
import static com.ge.research.sadl.preferences.SadlPreferences.GRAPH_IMPLICIT_ELEMENTS;
import static com.ge.research.sadl.preferences.SadlPreferences.GRAPH_IMPLICIT_ELEMENT_INSTANCES;
import static com.ge.research.sadl.preferences.SadlPreferences.GRAPH_RENDERER_CLASS;
import static com.ge.research.sadl.preferences.SadlPreferences.IGNORE_UNITTEDQUANTITIES;
import static com.ge.research.sadl.preferences.SadlPreferences.JENA_TDB;
import static com.ge.research.sadl.preferences.SadlPreferences.METRICS_QUERY_FILENAME;
import static com.ge.research.sadl.preferences.SadlPreferences.MODEL_NAMESPACES;
import static com.ge.research.sadl.preferences.SadlPreferences.N3_FORMAT;
import static com.ge.research.sadl.preferences.SadlPreferences.NAMESPACE_IN_QUERY_RESULTS;
import static com.ge.research.sadl.preferences.SadlPreferences.N_TRIPLE_FORMAT;
import static com.ge.research.sadl.preferences.SadlPreferences.OWL_MODEL_FORMAT;
import static com.ge.research.sadl.preferences.SadlPreferences.PREFIXES_ONLY_AS_NEEDED;
import static com.ge.research.sadl.preferences.SadlPreferences.P_USE_ARTICLES_IN_VALIDATION;
import static com.ge.research.sadl.preferences.SadlPreferences.RDF_XML_ABBREV_FORMAT;
import static com.ge.research.sadl.preferences.SadlPreferences.RDF_XML_FORMAT;
import static com.ge.research.sadl.preferences.SadlPreferences.SADL_BASE_URI;
import static com.ge.research.sadl.preferences.SadlPreferences.SADL_FILE_NAMES;
import static com.ge.research.sadl.preferences.SadlPreferences.SHOW_TIMING_INFORMATION;
import static com.ge.research.sadl.preferences.SadlPreferences.TABULAR_DATA_IMPORTER_CLASS;
import static com.ge.research.sadl.preferences.SadlPreferences.TYPE_CHECKING_RANGE_REQUIRED;
import static com.ge.research.sadl.preferences.SadlPreferences.TYPE_CHECKING_WARNING_ONLY;
import static com.ge.research.sadl.preferences.SadlPreferences.VALIDATE_BEFORE_TEST;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.xtext.ui.editor.preferences.LanguageRootPreferencePage;

import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.reasoner.ConfigurationItem;
import com.ge.research.sadl.reasoner.ConfigurationItem.NameValuePair;
import com.ge.research.sadl.reasoner.ConfigurationManager;
import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.ui.preferences.FieldEditorExtensions.BooleanFieldEditorExt;
import com.ge.research.sadl.ui.preferences.FieldEditorExtensions.FieldEditorExt;
import com.ge.research.sadl.ui.preferences.FieldEditorExtensions.FileFieldEditorExt;
import com.ge.research.sadl.ui.preferences.FieldEditorExtensions.RadioGroupFieldEditorExt;
import com.ge.research.sadl.ui.preferences.FieldEditorExtensions.StringFieldEditorExt;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.Lists;

@SuppressWarnings("restriction")
public class SadlRootPreferencePage extends LanguageRootPreferencePage {

	/**
	 * You can iterate on this, but <b>DO NOT</b> modify it!</br>
	 * It is a hack for https://github.com/eclipse/xtext-eclipse/issues/916.
	 */
	protected List<FieldEditor> editorsCopy = Lists.newArrayList();

	@Override
	public void dispose() {
		super.dispose();
		editorsCopy.clear();
	}

	@Override
	protected void addField(FieldEditor editor) {
		super.addField(editor);
		// We have to save our copy of the field editors.
		// Otherwise, we cannot perform a custom `updateFieldEditors(boolean)`.
		// `editors` is private in the super class.
		this.editorsCopy.add(editor);
	}
	
	@Override
	protected void adjustGridLayout() {
		Iterable<FieldEditorExt> fieldExts = FluentIterable.from(editorsCopy).filter(FieldEditorExt.class);
		Map<Composite, List<FieldEditorExt>> editorsPerGroup = StreamSupport.stream(fieldExts.spliterator(), false).collect(Collectors.groupingBy(FieldEditorExt::getParent));
		for (Composite group : editorsPerGroup.keySet()) {
			List<FieldEditorExt> editors = editorsPerGroup.get(group);
			int max = 0;
			if (editors != null) {
				for (FieldEditorExt editor : editors) {
					if (editor instanceof FieldEditor) {
						int numberOfControls = ((FieldEditor) editor).getNumberOfControls();
						if (numberOfControls > max) {
							max = numberOfControls;
						}
					}
				}
			}
			group.setLayout(new GridLayout(Math.max(max - 1, 1), false));
		}
	}

	@Override
	protected void updateFieldEditors(boolean enabled) {
		Composite parent = getFieldEditorParent();
		for (FieldEditor editor : editorsCopy) {
			editor.load();
			if (editor instanceof FieldEditorExt) {
				editor.setEnabled(enabled, ((FieldEditorExt) editor).getParent());
			} else {
				editor.setEnabled(enabled, parent);
			}
		}
		Button defaultsButton = getDefaultsButton();
		if (defaultsButton != null) {
			defaultsButton.setEnabled(enabled);
		}
	}

	protected Composite createSettingsGroup(Composite parent, int styles, String text) {
		Group group = new Group(parent, styles);
		group.setLayout(new GridLayout(1, false)); // We will adjust the columns later.
		group.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		group.setText(text);
		return group;
	}

	@Override
	protected void createFieldEditors() {
		// General SADL Settings
		Composite generalSettings = createSettingsGroup(getFieldEditorParent(), SWT.NONE, "General SADL Settings");
		addField(new StringFieldEditorExt(SADL_BASE_URI.getId(), "Base URI:", generalSettings));
		addField(new RadioGroupFieldEditorExt(OWL_MODEL_FORMAT.getId(), "Saved OWL model format :", 5,
				new String[][] {
					{ RDF_XML_ABBREV_FORMAT.getId(), RDF_XML_ABBREV_FORMAT.getId() },
					{ RDF_XML_FORMAT.getId(), RDF_XML_FORMAT.getId() },
					{ N3_FORMAT.getId(), N3_FORMAT.getId() },
					{ N_TRIPLE_FORMAT.getId(), N_TRIPLE_FORMAT.getId() },
					{ JENA_TDB.getId(), JENA_TDB.getId() },
				}, generalSettings));
		addField(new RadioGroupFieldEditorExt("importBy", "Show import model list as:", 2,
				new String[][] {
					{ "Model Namespaces", MODEL_NAMESPACES.getId() },
					{ "SADL File Names", SADL_FILE_NAMES.getId() }
				}, generalSettings));
		addField(new BooleanFieldEditorExt(PREFIXES_ONLY_AS_NEEDED.getId(), "Show prefixes for imported concepts only when needed for disambiguation", generalSettings));
		addField(new BooleanFieldEditorExt(SHOW_TIMING_INFORMATION.getId(), "Show Timing Informaton (Build, Reasoning)", generalSettings));
		addField(new RadioGroupFieldEditorExt("dmyOrder", "Interpret Date 10/11/2012 as:", 2,
				new String[][] {
					{ "MM/DD/YYYY", DMY_ORDER_MDY.getId() },
					{ "DD/MM/YYYY", DMY_ORDER_DMY.getId() }
				}, generalSettings));
		addField(new BooleanFieldEditorExt(CHECK_FOR_AMBIGUOUS_NAMES.getId(), "Check for ambiguous names", generalSettings));

		// Graph Settings
		Composite graphSettings = createSettingsGroup(getFieldEditorParent(), SWT.NONE, "Graphing Settings");
		addField(new StringFieldEditorExt(GRAPH_RENDERER_CLASS.getId(), "Graph renderer package and class:", graphSettings));
		addField(new BooleanFieldEditorExt(GRAPH_IMPLICIT_ELEMENTS.getId(), "&Include Implicit Elements in Graph", graphSettings));
		addField(new BooleanFieldEditorExt(GRAPH_IMPLICIT_ELEMENT_INSTANCES.getId(), "&Include Implicit Element Instances in Graph", graphSettings));

		// Inference and Querying Settings
		Composite inferenceSettings = createSettingsGroup(getFieldEditorParent(), SWT.NONE, "Inference and Querying Settings");
		addField(new BooleanFieldEditorExt(VALIDATE_BEFORE_TEST.getId(), "Validate before Testing", inferenceSettings));
//		addField(new BooleanFieldEditorExt(TEST_WITH_KSERVER.getId(), "Test/Query with Knowledge Server", inferenceSettings));
		addField(new BooleanFieldEditorExt(NAMESPACE_IN_QUERY_RESULTS.getId(), "Show Namespaces in Query Results", inferenceSettings));
		addField(new BooleanFieldEditorExt(DEEP_VALIDATION_OFF.getId(), "Disable Deep Validation of Model", inferenceSettings));
		addField(new StringFieldEditorExt(TABULAR_DATA_IMPORTER_CLASS.getId(), "Tabular data importer package and class:", inferenceSettings));

		// Metrics Settings
		Composite metricsSettings = createSettingsGroup(getFieldEditorParent(), SWT.NONE, "Metrics Settings");
		addField(new BooleanFieldEditorExt(GENERATE_METRICS_REPORT_ON_CLEAN_BUILD.getId(), "Generate metrics report during project clean/build", metricsSettings));
		addField(new FileFieldEditorExt(METRICS_QUERY_FILENAME.getId(), "File containing metric queries: ", metricsSettings));

		// Translation Settings
		Composite translationSettings = createSettingsGroup(getFieldEditorParent(), SWT.NONE, "Translation Settings");
		addField(new BooleanFieldEditorExt(P_USE_ARTICLES_IN_VALIDATION.getId(), "Use indefinite and definite articles in validation and translation", translationSettings));
		addField(new BooleanFieldEditorExt(FIND_AND_EXPAND_MISSING_PATTERNS.getId(), "Find and expand missing patterns in translation", translationSettings));
		addField(new BooleanFieldEditorExt(IGNORE_UNITTEDQUANTITIES.getId(), "Ignore Unitted Quantities (treat as numeric only) during translation", translationSettings));
		addField(new BooleanFieldEditorExt(CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES.getId(), "Translate multiple-class domain or range as union class (owl:unionOf)", translationSettings));

		// Type Checking Settings
		Composite typeCheckSettings = createSettingsGroup(getFieldEditorParent(), SWT.NONE, "Type Checking Settings");
		addField(new BooleanFieldEditorExt(CHECK_FOR_CARDINALITY_OF_PROPERTY_IN_DOMAIN.getId(), "Check for cardinality of property on specific domain", typeCheckSettings));
		addField(new BooleanFieldEditorExt(TYPE_CHECKING_RANGE_REQUIRED.getId(), "Property range specification required", typeCheckSettings));
		addField(new BooleanFieldEditorExt(TYPE_CHECKING_WARNING_ONLY.getId(), "Type checking issues as warning only", typeCheckSettings));
		
		// Content Assist Settings
		Composite contentAssistSettings = createSettingsGroup(getFieldEditorParent(), SWT.NONE, "Content Assist Settings");
		addField(new BooleanFieldEditorExt(CONTENT_ASSIST__FILTER_IMPLICIT_MODEL.getId(), "Filter SADL implicit model", contentAssistSettings));
	}

	@Override
	protected void performDefaults() {
		initializeDefaultPreferences();
		super.performDefaults();
	}

	private void initializeDefaultPreferences() {
		IPreferenceStore store = this.getPreferenceStore();

		//General Settings
		store.setDefault(SadlPreferences.CREATE_DOMAIN_AND_RANGE_AS_UNION_CLASSES.getId(), true);
		store.setDefault(SadlPreferences.TYPE_CHECKING_RANGE_REQUIRED.getId(), true);
		store.setDefault(SadlPreferences.GRAPH_RENDERER_CLASS.getId(), SadlPreferences.GRAPH_RENDERER_CLASS.getDefaultValue());
//		store.setDefault(RAEConstants.ANALYSIS_TIMEOUT,"600");
//		store.setDefault(RAEConstants.RUN_PARTIAL_ANALYSIS,false);
//		store.setDefault(RAEConstants.ENABLE_PREPROCESSING,true);
//		store.setDefault(RAEConstants.STOP_ON_ERROR,true);
//		store.setDefault(RAEConstants.PERFORM_SELECTIVE_ANALYSIS,false);
//		
//		//Types of Analyses
//		store.setDefault(RAEConstants.INNER_CONTINGENCY, true);
//		store.setDefault(RAEConstants.CONTINGENCY,true);
//		store.setDefault(RAEConstants.SELF_CONFLICT,true);
//		store.setDefault(RAEConstants.CONFLICT_PAIR,true);
//		store.setDefault(RAEConstants.HYPOTHESES_CONFLICT,true);
//		store.setDefault(RAEConstants.INDEPENDENCE,true);
//		store.setDefault(RAEConstants.COMPLETENESS,true);
//		store.setDefault(RAEConstants.HYPOTHESES_COMPLETENESS,true);
//		store.setDefault(RAEConstants.SURJECTIVITY,true);
//		store.setDefault(RAEConstants.GLOBAL_CONTINGENCY,true);
		
		setPreferenceStore(store);
	}

	@Override
	public boolean performOk() {
		boolean retVal = super.performOk();
		
		if (retVal && isPropertyPage()) {
			// the changes apply only to the current project
			IPreferencesService service = Platform.getPreferencesService();
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
//			if (visitor.getCurrentResource() != null) {
//				try {
//					String curResource = ResourceManager.getOwlModelsFolder(visitor.getCurrentResource().getURI());
//					IConfigurationManager cmgr = visitor.getConfigurationMgr(curResource.toString());
//					if (cmgr != null) {
//						cmgr.getModelGetter().setFormat(format);
//					}
//					String dmyOrder = service.getString("com.ge.research.sadl.Sadl", 
//							IConfigurationManager.dmyOrder, IConfigurationManager.dmyOrderMDY, null);
//					if (dmyOrder.equals(IConfigurationManager.dmyOrderDMY)) {
//						DateTimeConfig.getGlobalDefault().setDmyOrder(true);
//					}
//					else {
//						DateTimeConfig.getGlobalDefault().setDmyOrder(false);
//					}
//					if (cmgr != null && cmgr instanceof ConfigurationManagerForEditing) {
//						// TODO
//						// put date format in configuration
//						String[] itemContent = new String[1];
//						itemContent[0] = IConfigurationManager.DateFormat;
//						ConfigurationItem ci = new ConfigurationItem(itemContent);
//						NameValuePair nvp = ci.new NameValuePair(IConfigurationManager.dmyOrder, dmyOrder);
//						ci.addNameValuePair(nvp);
//						((ConfigurationManagerForEditing)cmgr).addConfiguration(ci);
//						((IConfigurationManagerForIDE)cmgr).saveConfiguration();
//					}
//				} catch (ConfigurationException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				} catch (URISyntaxException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				} catch (IOException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				}
//			}
		}
		else {
			// the changes apply to all projects
			IPreferencesService service = Platform.getPreferencesService();
			String format = service.getString("com.ge.research.sadl.Sadl", "OWL_Format", ConfigurationManager.RDF_XML_ABBREV_FORMAT, null);
			String dmyOrder = service.getString("com.ge.research.sadl.Sadl", "dmyOrder", "mdy", null);
			String[] itemContent = new String[1];
			itemContent[0] = IConfigurationManager.DateFormat;
			ConfigurationItem ci = new ConfigurationItem(itemContent);
			NameValuePair nvp = ci.new NameValuePair(IConfigurationManager.dmyOrder, dmyOrder);
			ci.addNameValuePair(nvp);
//			Enumeration<IConfigurationManagerForIDE> cmgrs = visitor.getConfigurationManagers();
//			while (cmgrs.hasMoreElements()) {
//				IConfigurationManagerForIDE cmgr = cmgrs.nextElement();
//				cmgr.getModelGetter().setFormat(format);
//				if (dmyOrder.equals(IConfigurationManager.dmyOrderDMY)) {
//					DateTimeConfig.getGlobalDefault().setDmyOrder(true);
//				}
//				else {
//					DateTimeConfig.getGlobalDefault().setDmyOrder(false);
//				}
//				if (cmgr != null && cmgr instanceof IConfigurationManagerForIDE) {
//					// put date format in configuration
//					try {
//						((IConfigurationManagerForIDE)cmgr).addConfiguration(ci);
//						((IConfigurationManagerForIDE)cmgr).saveConfiguration();
//					} catch (ConfigurationException e) {
//						// TODO Auto-generated catch block
//						e.printStackTrace();
//					}
//				}
//			}
		}
		return retVal;
	}

}
