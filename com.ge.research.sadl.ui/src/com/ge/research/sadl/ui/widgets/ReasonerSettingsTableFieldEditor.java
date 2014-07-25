package com.ge.research.sadl.ui.widgets;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

public class ReasonerSettingsTableFieldEditor extends FieldEditor {

	Table table;
	String name;
	String[] titles = { "Category", "Setting", "Value", "Description" };
	String reaonserName = null;

	public ReasonerSettingsTableFieldEditor(String name, String label,
			Composite parent) {
		init(name, label);
		createControl(parent);
	}

	@Override
	protected void adjustForNumColumns(int numColumns) {
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.horizontalSpan = numColumns;
		gd.heightHint = 200;
		table.setLayoutData(gd);
		if (table != null) {
			table.setLayoutData(gd);
			packColumns();
		}
	}

	protected void packColumns() {
		for (int i = 0; i < titles.length; i++) {
			table.getColumn(i).pack();
		}
	}

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		table = new Table(parent, SWT.MULTI | SWT.BORDER | SWT.FULL_SELECTION);
		table.setLinesVisible(true);
		table.setHeaderVisible(true);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.horizontalSpan = numColumns;
		gd.heightHint = 200;
		table.setLayoutData(gd);
		table.setFont(parent.getFont());

		for (int i = 0; i < titles.length; i++) {
			TableColumn column = new TableColumn(table, SWT.NONE);
			column.setResizable(true);
			column.setText(titles[i]);
			column.setWidth(300);
		}
		
		packColumns();
		updateCellControls();

	}

	private void updateCellControls() {
		final TableItem[] items = table.getItems();
		for (int i = 0; i < items.length; i++) {
			TableEditor editor = new TableEditor(table);
			final Text text = new Text(table, SWT.NONE);
			text.setText(items[i].getText(2));
			final int index = i;
			text.addModifyListener(new ModifyListener() {

				@Override
				public void modifyText(ModifyEvent e) {
					items[index].setText(2, text.getText());

				}
			});
			editor.grabHorizontal = true;
			editor.setEditor(text, items[i], 2);
		}

	}

	@Override
	protected void doLoad() {
		TableItem item = new TableItem(table, SWT.NONE);
		IPreferenceStore pStore = getPreferenceStore();
		item.setText(0, "Top Level -> Lower Level");
		item.setText(1, "x");
		item.setText(2, pStore.getString("Top Level -> Lower Level -> x"));
		item.setText(3, "This is the description for the x setting");
		updateCellControls();
	}

	@Override
	protected void doLoadDefault() {

	}

	@Override
	protected void doStore() {
		IPreferenceStore pStore = getPreferenceStore();
		int numRows = table.getItemCount();
		for (int i = 0; i < numRows; i++) {
			String cat = table.getItem(i).getText(0);
			String name = table.getItem(i).getText(1);
			String value = table.getItem(i).getText(2);
			String conName = cat + " -> " + name;
			pStore.setValue(conName, value);
		}
	}

	@Override
	public int getNumberOfControls() {
		return 1;
	}

	public String getReaonserName() {
		return reaonserName;
	}

	public void setReaonserName(String reaonserName) {
		this.reaonserName = reaonserName;
	}



}
