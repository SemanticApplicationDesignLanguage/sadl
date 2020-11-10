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

import { injectable, inject } from 'inversify';
import URI from '@theia/core/lib/common/uri';
import { FileSystem, FileStat } from '@theia/filesystem/lib/common/filesystem';
import { EditorManager } from '@theia/editor/lib/browser';
import { UriSelection } from '@theia/core/lib/common/selection';
import { SelectionService } from '@theia/core/lib/common/selection-service';
import { FileNavigatorContribution } from '@theia/navigator/lib/browser/navigator-contribution';
import { Command, CommandContribution, CommandRegistry, MessageService } from '@theia/core/lib/common';
import { ApplicationShell, SingleTextInputDialog, SelectableTreeNode, ExpandableTreeNode } from '@theia/core/lib/browser';
import { OutputContribution } from '@theia/output/lib/browser/output-contribution';
import { UriAwareCommandHandler } from '@theia/core/lib/common/uri-command-handler';
import { TreeWidgetSelection } from '@theia/core/lib/browser/tree/tree-widget-selection';
import { SadlLanguageClientContribution } from './sadl-language-client-contribution';
import { SadlReloadExternals } from './protocol/sadl-reload-externals';
import { SadlRunQuery } from './protocol/sadl-run-query';
import { SadlTestModel } from './protocol/sadl-test-model';
import { SadlProjectRoot } from './protocol/sadl-project-root';
import { DefaultSadlConfigSchema } from './sadl-preferences';
import { ILanguageClient } from '@theia/languages/lib/browser';

const validFilename: (arg: string) => boolean = require('valid-filename');

@injectable()
export class SadlCommandContribution implements CommandContribution {

    @inject(EditorManager)
    protected readonly editorManager: EditorManager;

    @inject(SelectionService)
    protected readonly selectionService: SelectionService;

    @inject(OutputContribution)
    protected readonly outputContribution: OutputContribution;

    @inject(MessageService)
    protected readonly messageService: MessageService;

    @inject(ApplicationShell)
    protected readonly applicationShell: ApplicationShell;

    @inject(FileSystem)
    protected readonly fileSystem: FileSystem;

    @inject(SadlLanguageClientContribution)
    protected readonly languageClientContribution: SadlLanguageClientContribution;

    @inject(FileNavigatorContribution)
    protected readonly navigatorContribution: FileNavigatorContribution;

    registerCommands(registry: CommandRegistry): void {
        // XXX We have to always return `true` from `isEnabled`. We ca not run `async` code here.
        registry.registerCommand(SadlCommands.TEST_MODEL, {
            execute: () => this.testModel(),
            isEnabled: () => true
        });
        registry.registerCommand(SadlCommands.RUN_QUERY, {
            execute: async () => this.runQuery(),
            isEnabled: () => true
        });
        registry.registerCommand(SadlCommands.RELOAD_EXTERNALS, {
            execute: async () => this.reloadExternals(),
            isEnabled: () => true
        });
        registry.registerCommand(SadlCommands.NEW_SADL_PROJECT, new UriAwareCommandHandler<URI>(this.selectionService, {
            execute: async uri => {
                if (await this.inSadlProject(uri)) {
                    this.messageService.warn('Nested SADL projects are not supported.');
                } else {
                    const parent = await this.directoryFor(uri);
                    if (parent) {
                        const parentUri = new URI(parent.uri);
                        const vacantChildUri = this.findVacantChildUri(parentUri, parent, 'Untitled');
                        const dialog = new SingleTextInputDialog({
                            title: 'New SADL Project',
                            initialValue: vacantChildUri.path.base,
                            validate: name => this.validateFileName(name, parent)
                        });
                        try {
                            const name = await dialog.open();
                            if (name) {
                                const desiredUri = parentUri.resolve(name);
                                await this.fileSystem.createFolder(desiredUri.toString())
                                const content = this.dotProjectContent(desiredUri.path.base);
                                await this.fileSystem.createFile(desiredUri.resolve('.project').toString(), { content });
                                await this.fileSystem.createFile(desiredUri.resolve('settings.json').toString(), { content: DefaultSadlConfigSchema.content() });
                                const widget = await this.navigatorContribution.widget;
                                const fileTreeModel = widget.model;
                                const parent = fileTreeModel.getNode(parentUri.toString());
                                const toDisposeAfterExpand = fileTreeModel.onChanged(async () => {
                                    const node = fileTreeModel.getNode(desiredUri.toString());
                                    if (node) {
                                        if (SelectableTreeNode.is(node) && !SelectableTreeNode.isSelected(node)) {
                                            fileTreeModel.selectNode(node);
                                        }
                                        if (ExpandableTreeNode.is(node) && ExpandableTreeNode.isCollapsed(node)) {
                                            await fileTreeModel.expandNode(node);
                                        }
                                        toDisposeAfterExpand.dispose();
                                    }
                                });
                                if (ExpandableTreeNode.is(parent) && ExpandableTreeNode.isCollapsed(parent)) {
                                    await fileTreeModel.expandNode(parent);
                                }
                            }
                        } catch {
                            // Dialog cancel.
                        }
                    }
                }
            },
            isEnabled: () => true
        }))
    }

    protected async testModel(): Promise<void> {
        const uri = this.selectedUri();
        if (uri && await this.isSadl(uri)) {
            const languageClient = await this.languageClient();
            await this.revealOutput();
            await languageClient.sendNotification(SadlTestModel.TYPE, { uri: uri.toString() });
        } else {
            this.messageService.warn('Only SADL models can be tested.');
        }
    }

    protected async runQuery(): Promise<void> {
        const uri = this.selectedUri();
        if (uri && await this.isSadl(uri)) {
            const languageClient = await this.languageClient();
            const dialog = new SingleTextInputDialog({ title: 'Run Query' });
            try {
                const query = await dialog.open();
                if (query) {
                    await this.revealOutput();
                    await languageClient.sendNotification(SadlRunQuery.TYPE, { uri: uri.toString(), query });
                }
            } catch {
                // Dialog cancel.
            }
        } else {
            this.messageService.warn('Can run query only against SADL models.');
        }
    }

    protected async reloadExternals(): Promise<void> {
        const uri = this.selectedUri();
        if (uri && await this.isExternalDefinition(uri)) {
            const languageClient = await this.languageClient();
            // XXX: The result of the request is currently unused.
            await languageClient.sendRequest(SadlReloadExternals.TYPE, { uri: uri.toString() });
        } else {
            this.messageService.warn('Only external model definitions can be used to download external OWL models.');
        }
    }

    protected selectedUri(): URI | undefined {
        const { selection } = this.selectionService;
        return this.toUri(selection);
    }

    protected activeEditorUri(): URI | undefined {
        const { activeEditor } = this.editorManager;
        if (activeEditor) {
            return activeEditor.editor.uri;
        }
        return undefined;
    }

    protected async inSadlProject(uri: URI | undefined): Promise<boolean> {
        if (uri) {
            const languageClient = await this.languageClient();
            const result = await languageClient.sendRequest(SadlProjectRoot.TYPE, { uri: uri.toString() });
            return !!result.uri;
        }
        return false;
    }

    protected async revealOutput(): Promise<void> {
        await this.outputContribution.openView({ reveal: true });
    }

    protected async isSadl(uri: URI | undefined): Promise<boolean> {
        return !!uri && this.inSadlProject(uri) && uri.toString().endsWith('.sadl');
    }

    protected async isExternalDefinition(uri: URI | undefined): Promise<boolean> {
        return !!uri && this.inSadlProject(uri) && uri.toString().endsWith('.url');
    }

    protected dotProjectContent(projectName: string): string {
        return `<?xml version="1.0" encoding="UTF-8"?>
<projectDescription>
    <name>${projectName}</name>
    <comment></comment>
    <projects>
    </projects>
    <buildSpec>
        <buildCommand>
            <name>org.eclipse.xtext.ui.shared.xtextBuilder</name>
            <arguments>
            </arguments>
        </buildCommand>
    </buildSpec>
    <natures>
        <nature>org.eclipse.xtext.ui.shared.xtextNature</nature>
    </natures>
</projectDescription>`;
    }

    protected async directoryFor(uri: URI): Promise<FileStat | undefined> {
        const stat = await this.fileSystem.getFileStat(uri.toString());
        if (stat && stat.isDirectory) {
            return stat;
        }
        return this.parentOf(uri);
    }

    protected parentOf(uri: URI): Promise<FileStat | undefined> {
        return this.fileSystem.getFileStat(uri.parent.toString());
    }

    protected validateFileName(name: string, parent: FileStat): string {
        if (!validFilename(name)) {
            return 'Invalid file name, try another.';
        }
        if (parent.children) {
            for (const child of parent.children) {
                if (new URI(child.uri).path.base === name) {
                    return 'A file with this name already exists.';
                }
            }
        }
        return '';
    }

    protected findVacantChildUri(parentUri: URI, parent: FileStat, name: string, ext: string = ''): URI {
        const children = !parent.children ? [] : parent.children!.map(child => new URI(child.uri));
        let index = 1;
        let base = name + ext;
        while (children.some(child => child.path.base === base)) {
            index = index + 1;
            base = name + '_' + index + ext;
        }
        return parentUri.resolve(base);
    }

    protected toUri(arg: any): URI | undefined {
        if (arg instanceof URI) {
            return arg;
        }
        if (UriSelection.is(arg)) {
            return arg.uri;
        }
        if (TreeWidgetSelection.is(arg) && arg.length === 1) {
            return this.toUri(arg[0]);
        }
        if ('uri' in arg) {
            const uri = arg.uri;
            if (uri instanceof URI) {
                return uri;
            } else if (typeof uri === 'string') {
                return new URI(uri);
            }
        }
        return undefined;
    }

    protected async languageClient(): Promise<ILanguageClient> {
        const languageClient = await this.languageClientContribution.languageClient;
        await languageClient.onReady();
        return languageClient;
    }

}

export namespace SadlCommands {

    export const TEST_MODEL: Command = {
        id: 'sadl.testModel',
        label: 'SADL: Test Model'
    };

    export const RUN_QUERY: Command = {
        id: 'sadl.runQuery',
        label: 'SADL: Run Query'
    };

    export const RELOAD_EXTERNALS: Command = {
        id: 'sadl.reloadExternals',
        label: 'SADL: Download External Models'
    }

    export const NEW_SADL_PROJECT: Command = {
        id: 'sadl.newProject'
    }

}
