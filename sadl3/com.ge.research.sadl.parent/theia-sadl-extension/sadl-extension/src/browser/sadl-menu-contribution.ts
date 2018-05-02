/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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

import { injectable } from 'inversify';
import { MAIN_MENU_BAR } from '@theia/core/lib/common/menu';
import { MenuContribution, MenuModelRegistry } from '@theia/core/lib/common';
import { NAVIGATOR_CONTEXT_MENU, NavigatorContextMenu } from '@theia/navigator/lib/browser/navigator-contribution';
import { EDITOR_CONTEXT_MENU } from '@theia/editor/lib/browser/editor-menu';
import { SadlCommands } from './sadl-command-contribution';

@injectable()
export class SadlMenuContribution implements MenuContribution {

    registerMenus(registry: MenuModelRegistry) {
        // Main menu
        registry.registerSubmenu(SadlMainMenu.SADL, 'SADL');
        registry.registerMenuAction(SadlMainMenu.INFERENCER_GROUP, {
            commandId: SadlCommands.TEST_MODEL.id,
            label: 'Test Model',
            order: '1'
        });
        registry.registerMenuAction(SadlMainMenu.INFERENCER_GROUP, {
            commandId: SadlCommands.RUN_QUERY.id,
            label: 'Run Query',
            order: '2'
        });
        registry.registerMenuAction(SadlMainMenu.EXTERNALS_GROUP, {
            commandId: SadlCommands.RELOAD_EXTERNALS.id,
            label: 'Download External Models'
        });

        // Navigator context menu
        registry.registerMenuAction(NavigatorContextMenu.NEW, {
            commandId: SadlCommands.NEW_SADL_PROJECT.id,
            label: 'New SADL Project'
        })

        // Editor context menu
        registry.registerMenuAction(SadlEditorContextMenu.SADL_EDITOR_CONTEXT, {
            commandId: SadlCommands.TEST_MODEL.id,
            label: 'Test Model',
            order: '1'
        });
        registry.registerMenuAction(SadlEditorContextMenu.SADL_EDITOR_CONTEXT, {
            commandId: SadlCommands.RUN_QUERY.id,
            label: 'Run Query',
            order: '2'
        });
        registry.registerMenuAction(SadlEditorContextMenu.SADL_EDITOR_CONTEXT, {
            commandId: SadlCommands.RELOAD_EXTERNALS.id,
            label: 'Download External Models',
            order: '3'
        });
    }

}

export namespace SadlMainMenu {

    /**
     * The main `SADL` menu item.
     */
    export const SADL = [...MAIN_MENU_BAR, '3_sadl'];

    /**
     * `Inferencer` menu group in the `SADL` menu.
     */
    export const INFERENCER_GROUP = [...SADL, '1_inferencer'];

    /**
     * `Externals` menu group in the `SADL` menu.
     */
    export const EXTERNALS_GROUP = [...SADL, '2_externals'];

}

export namespace SadlNavigatorContextMenu {

    /**
     * Navigator context menu for SADL.
     */
    export const SADL = [...NAVIGATOR_CONTEXT_MENU, '9_sadl'];

}

export namespace SadlEditorContextMenu {

    /**
     * SADL specific editor context menu.
     */
    export const SADL_EDITOR_CONTEXT = [...EDITOR_CONTEXT_MENU, '9_sadl'];

}