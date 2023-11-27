/************************************************************************
 * Copyright © 2007-2018 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui.preferences

import org.eclipse.jface.preference.BooleanFieldEditor
import org.eclipse.jface.preference.FileFieldEditor
import org.eclipse.jface.preference.RadioGroupFieldEditor
import org.eclipse.jface.preference.StringFieldEditor
import org.eclipse.swt.widgets.Composite
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.jface.preference.ComboFieldEditor

/**
 * This class provides a workaround for the following issues:
 * <ul>
 * <li>https://github.com/eclipse/xtext-eclipse/issues/238</li>
 * <li>https://github.com/eclipse/xtext-eclipse/issues/916</li>
 * </ul>
 */
final class FieldEditorExtensions {

	static interface FieldEditorExt {
		def Composite getParent();
	}

	static class StringFieldEditorExt extends StringFieldEditor implements FieldEditorExt {

		@Accessors(PUBLIC_GETTER)
		val Composite parent

		new(String name, String labelText, Composite parent) {
			super(name, labelText, parent);
			this.parent = parent;
		}

		override protected doStore() {
			preferenceStore.putValue(preferenceName, textControl.text);
		}

	}

	static class FileFieldEditorExt extends FileFieldEditor implements FieldEditorExt {

		@Accessors(PUBLIC_GETTER)
		val Composite parent

		new(String name, String labelText, Composite parent) {
			super(name, labelText, parent);
			this.parent = parent;
		}

		override protected doStore() {
			preferenceStore.putValue(preferenceName, textControl.text);
		}

		override getNumberOfControls() {
			return 1;
		}

		override void doFillIntoGrid(Composite parent, int numColumns) {
			super.doFillIntoGrid(parent, 1);
		}

	}

	static class BooleanFieldEditorExt extends BooleanFieldEditor implements FieldEditorExt {

		@Accessors(PUBLIC_GETTER)
		val Composite parent

		new(String name, String labelText, Composite parent) {
			super(name, labelText, parent);
			this.parent = parent;
		}

		override protected doStore() {
			preferenceStore.putValue(preferenceName, Boolean.toString(booleanValue));
		}

	}

	static class RadioGroupFieldEditorExt extends RadioGroupFieldEditor implements FieldEditorExt {

		@Accessors(PUBLIC_GETTER)
		val Composite parent

		new(String name, String labelText, int numColumns, String[][] labelAndValues, Composite parent) {
			super(name, labelText, numColumns, labelAndValues, parent, false);
			this.parent = parent;
		}

		override protected doStore() {
			val field = RadioGroupFieldEditor.getDeclaredField('value');
			field.accessible = true;
			val value = field.get(this);
			if (value instanceof String) {
				preferenceStore.putValue(preferenceName, value);
			} else {
				preferenceStore.toDefault = preferenceName;
			}
		}

	}

	static class ComboFieldEditorExt extends ComboFieldEditor implements FieldEditorExt {
		@Accessors(PUBLIC_GETTER)
		val Composite parent
		
		new(String name, String labelText, String[][] labelAndValues, Composite parent) {
			super(name, labelText, labelAndValues, parent)
			this.parent = parent
		}
		
		override protected doStore() {
			val field = ComboFieldEditor.getDeclaredField('fValue');
			field.accessible = true;
			val value = field.get(this);
			if (value instanceof String) {
				preferenceStore.putValue(preferenceName, value);
			} else {
				preferenceStore.toDefault = preferenceName;
			}
		}
	}
}
