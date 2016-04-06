package com.ge.research.sadl.perspective.perspectives;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.internal.WorkbenchWindow;

@SuppressWarnings("restriction")
public class SADL implements IPerspectiveFactory {
	private IPageLayout factory;

	@Override
	public void createInitialLayout(IPageLayout factory) {
		this.factory = factory;

		IWorkbenchWindow window = Workbench.getInstance()
				.getActiveWorkbenchWindow();

		if (window instanceof WorkbenchWindow) {
			window = Workbench.getInstance().getActiveWorkbenchWindow();
			MenuManager menuManager = ((WorkbenchWindow) window)
					.getMenuManager();

			String[] itemIds = { "Navigate", "Help" };
			for (String itemId : itemIds) {
				// represents a contribution to a shared UI resource
				IContributionItem item = menuManager.find(itemId);
				// if the item is not null then set it to not visible?
				if (item != null) {
					item.setVisible(false);
					menuManager.update();

				}

			}
		}
		addViews();
		addActionSets();
		addNewWizardShortcuts();
		addPerspectiveShortcuts();
		addViewShortcuts();
	}

	private void addViews() {

		// creates the overall folder layout
		// note that each new Folder uses a percentage of the remaining
		// EditorArea
		IFolderLayout bottom = factory.createFolder("bottomRight", // NON-NLS-1
				IPageLayout.BOTTOM, 0.75f, factory.getEditorArea());
		bottom.addView(IPageLayout.ID_PROBLEM_VIEW);

		bottom.addView(IConsoleConstants.ID_CONSOLE_VIEW);
		IFolderLayout topLeft = factory.createFolder("topLeft", // NON-NLS-1
				IPageLayout.LEFT, 0.25f, factory.getEditorArea());
		topLeft.addView(IPageLayout.ID_PROJECT_EXPLORER);

	}

	private void addActionSets() {

		// factory.addActionSet(JavaUI.ID_ACTION_SET);
		// factory.addActionSet(JavaUI.ID_ELEMENT_CREATION_ACTION_SET);
		factory.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET); // NON-NLS-1
	}

	private void addPerspectiveShortcuts() {

		factory.addPerspectiveShortcut("test.perspectives.SADL_Perspective"); // NON-NLS-1
	}

	private void addNewWizardShortcuts() {

		// factory.addNewWizardShortcut("org.eclipse.team.cvs.ui.newProjectCheckout");//NON-NLS-1
		// factory.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");//NON-NLS-1
		// factory.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");//NON-NLS-1
		factory.addNewWizardShortcut("test.wizards.SadlProject");// NON-NLS-1
		factory.addNewWizardShortcut("test.wizards.SADL_File");// NON-NLS-1
		factory.addNewWizardShortcut("test.wizards.REQ_File");
	}

	private void addViewShortcuts() {

		factory.addShowViewShortcut(IConsoleConstants.ID_CONSOLE_VIEW);
		factory.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
		factory.addShowViewShortcut(IPageLayout.ID_OUTLINE);
	}

}