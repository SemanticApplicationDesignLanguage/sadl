import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Treebeard, TreeNode, decorators } from 'react-treebeard';
import { ContextMenu, MenuItem, ContextMenuTrigger } from "react-contextmenu";

import {
    File
} from '../workspace';

import * as protocol from 'vscode-languageclient/lib/protocol';


export function setOnRename(func: (old:string, newUri:string) => void):void {
    doRename = func
}

let doRename: (old:string, newUri:string) => void = null;

export class FileNode implements TreeNode {

    readonly file: File
    readonly name: string

    readonly parent?: FileNode
    readonly children?: TreeNode[]

    toggled?: boolean;
    loading?: boolean;

    constructor(file: File, parent?: FileNode) {
        this.file = file;
        this.parent = parent;
        this.name = this.getName(file);
        if (file.directory) {
            this.toggled = !!file.children;
            this.children = this.getChildren(file);
        }
    }

    static is(node: TreeNode | null | undefined): node is FileNode {
        return node instanceof FileNode || (!!node && ('file' in node));
    }

    getName(file: File): string {
        const path = file.uri.replace(/\/$/, "");
        const index = path.lastIndexOf('/');
        return index === -1 ? path : path.substring(index + 1);
    }

    getChildren(file: File): FileNode[] {
        if (file.children) {
            const children = file.children.map(f => new FileNode(f, this));
            return children.sort((n, n2) => {
                if (!n.file.directory && n2.file.directory) {
                    return 1;
                }
                if (n.file.directory && !n2.file.directory) {
                    return -1;
                }
                return n.name.localeCompare(n2.name);
            });
        }
        return [];
    }

}

export type CreateElement = (parent: File, name: string) => void;

export class NewElementNode implements TreeNode {
    name: string;

    constructor(
        readonly parent: FileNode,
        protected createElement: CreateElement
    ) {
        this.name = '';
    }

    create() {
        if (this.parent.children) {
            this.parent.children.splice(this.parent.children.indexOf(this));
            if (this.name.length !== 0) {
                this.createElement(this.parent.file, this.name);
            }
        }
    }

}

export interface IExplorerProps {
    // FIXME: get rid of mounting methods by making Explorer implement IExplorer interface
    readonly onDidMount?: (explorer: Explorer) => void;
    readonly onWillUnmount?: (explorer: Explorer) => void;

    readonly onOpenFile?: (file: File) => void;
    readonly onOpenFolder?: (file: File) => PromiseLike<File | null>;
    readonly onNewFile?: CreateElement;
    readonly onNewFolder?: CreateElement;
    readonly onDelete?: (file: File) => void;
    readonly onRename?: (oldName: string, newName:string) => void;
}

export interface IExplorerState {
    rootNode?: TreeNode | FileNode
    selectedNode?: TreeNode
}

export namespace ExplorerMenu {
    export type FileAction = 'open' | 'delete';
    export type FolderAction = 'newFile' | 'newFolder' | 'delete';
    export type Action = FileAction | FolderAction;
    export interface ItemData {
        action: Action;
    }
    export function toItemName(action: Action): string {
        if (action === 'open') {
            return 'Open';
        }
        if (action === 'delete') {
            return 'Delete';
        }
        if (action === 'newFile') {
            return 'New File';
        }
        if (action === 'newFolder') {
            return 'New Folder';
        }
        throw new Error('Unknown action: ' + action);
    }
    export class Item extends MenuItem<ItemData> { }
}

const fileContextMenuId = 'fileContextMenu';
const folderContextMenuId = 'folderContextMenu';

export class FileNodeContainer extends decorators.Container {
    render() {
        const node = this.props.node;
        if (node.name == "unnamed") {
            return <form onSubmit={(e) => {
                e.preventDefault();
                this.rename(e, node);
            }} >
                <input ref="nameInput"/>
            </form>
        } else if (FileNode.is(node)) {
            const contextMenuId = node.file.directory ? folderContextMenuId : fileContextMenuId;
            return <ContextMenuTrigger id={contextMenuId}>
                {super.render()}
            </ContextMenuTrigger>
        }
        return super.render();
    }

    rename(event: React.FormEvent<HTMLElement>, node: TreeNode) {
        const oldUri = (node as FileNode).file.uri
        const newName = (event.currentTarget.firstChild as HTMLInputElement).value;
        const newUri = oldUri.substr(0,oldUri.lastIndexOf("/")+1) + newName
        doRename(oldUri, newUri);
    }

    componentDidMount() {
        const element = ReactDOM.findDOMNode<HTMLElement>(this);
        const node = this.props.node;
        if (node instanceof NewElementNode) {
            element.focus();
        } else if (FileNode.is(node)) {
            // FIXME: it is a hack but don't have an idea how to propogate a node to a context menu otherwise
            (element as any).node = node;
        }
    }
}

const fileNodeDecorators = {
    Loading: decorators.Loading,
    Toggle: decorators.Toggle,
    Header: decorators.Header,
    Container: FileNodeContainer
};

export class Explorer extends React.Component<IExplorerProps, IExplorerState> {

    constructor() {
        super();
        this.state = {
            rootNode: { name: 'NO FOLDER OPENED' }
        };
    }

    componentDidMount() {
        if (this.props.onDidMount) {
            this.props.onDidMount(this);
        }
    }

    componentWillUnmount() {
        if (this.props.onWillUnmount) {
            this.props.onWillUnmount(this);
        }
    }

    render() {
        return <div>
            <Treebeard
                data={this.state.rootNode!}
                onToggle={(node, toggled) => this.onToggle(node, toggled)}
                decorators={fileNodeDecorators}
                style={{
                    tree: {
                        base: {
                            listStyle: 'none',
                            margin: 0,
                            padding: 0,
                            fontFamily: 'Menlo ,Monaco, "Courier New", monospace',
                            fontSize: '12px'
                        },
                        node: {
                            base: {
                                position: 'relative'
                            },
                            link: {
                                cursor: 'pointer',
                                position: 'relative',
                                padding: '0px 5px',
                                display: 'block'
                            },
                            activeLink: {
                                background: '#999999'
                            },
                            toggle: {
                                base: {
                                    position: 'relative',
                                    display: 'inline-block',
                                    verticalAlign: 'top',
                                    marginLeft: '-5px',
                                    height: '24px',
                                    width: '24px'
                                },
                                wrapper: {
                                    position: 'absolute',
                                    top: '50%',
                                    left: '50%',
                                    margin: '-7px 0 0 -7px',
                                    height: '12px'
                                },
                                height: 12,
                                width: 12,
                                arrow: {
                                    fill: '#9DA5AB',
                                    height: 9,
                                    strokeWidth: 0
                                }
                            },
                            header: {
                                base: {
                                    display: 'inline-block',
                                    verticalAlign: 'top',
//                                    color: '#9DA5AB'
                                },
                                connector: {
                                    width: '2px',
                                    height: '12px',
                                    borderLeft: 'solid 2px black',
                                    borderBottom: 'solid 2px black',
                                    position: 'absolute',
                                    top: '0px',
                                    left: '-21px'
                                },
                                title: {
                                    lineHeight: '24px',
                                    verticalAlign: 'middle'
                                }
                            },
                            subtree: {
                                listStyle: 'none',
                                paddingLeft: '19px'
                            },
                            loading: {
//                                color: '#E2C089'
                            }
                        }
                    }
                }}
                />
            <ContextMenu id={fileContextMenuId}>
                {this.renderMenuItem('open')}
                {this.renderMenuItem('delete')}
            </ContextMenu>
            <ContextMenu id={folderContextMenuId}>
                {this.renderMenuItem('newFile')}
                {this.renderMenuItem('newFolder')}
                {this.renderMenuItem('delete')}
            </ContextMenu>
        </div>;
    }

    renderMenuItem(action: ExplorerMenu.Action) {
        return <ExplorerMenu.Item data={{ action }}
            onClick={
                (event, data, target) => this.onMenuItemClick(event, data, target)
            }
            >{ExplorerMenu.toItemName(action)}</ExplorerMenu.Item>;
    }

    open(file: File) {
        this.setState({
            rootNode: new FileNode(file)
        });
    }

    update() {
        this.setState({});
    }

    handleFileEvent(events: protocol.FileEvent[]) {
        const node = (this.state.rootNode as FileNode)
        this.props.onOpenFolder(node.file).then(file => {
            this.setState({
                rootNode: new FileNode(file)
            });
            return node;
        });
    }

    find(node: FileNode, uri: string) : FileNode | null {
        if (uri === node.file.uri) {
            return node
        } else if (uri.substring(0, node.file.uri.length) === node.file.uri) {
            for (let c of node.children) {
                const candidate = this.find(c as FileNode, uri);
                if (candidate !== null) 
                    return candidate;
            }
        }
        return null
    }


    merge(node: FileNode, file: File): FileNode | null {
        const parent = node.parent;
        if (parent && parent.children) {
            const newChild = new FileNode(file, parent);
            const index = parent.children.indexOf(node);
            parent.children[index] = newChild;
            return newChild;
        }
        return null;
    }

    expand(node: FileNode): PromiseLike<FileNode> {
        node.toggled = true;
        if (!node.file.children && this.props.onOpenFolder) {
            node.loading = true;
            return this.props.onOpenFolder(node.file).then(file => {
                node.loading = false;
                if (file) {
                    return this.merge(node, file) || node;
                }
                return node;
            });
        }
        return Promise.resolve(node);
    }

    onToggle(node: TreeNode, toggled: boolean): void {
        if (this.state.selectedNode) {
            this.state.selectedNode.active = false;
        }
        node.active = true;

        if (FileNode.is(node)) {
            if (node.file.directory) {
                node.toggled = toggled;
                if (toggled) {
                    this.expand(node).then(() => this.update());
                }
            } else if (this.props.onOpenFile) {
                this.props.onOpenFile(node.file);
            }
        }

        this.setState({
            selectedNode: node
        });
    }

    onMenuItemClick(event: MouseEvent, data: ExplorerMenu.ItemData, target: Element): void {
        const node = this.state.selectedNode as FileNode;
        if (node) {
            if (data.action === 'open' && this.props.onOpenFile) {
                this.props.onOpenFile(node.file);
            } else if (data.action === 'delete' && this.props.onDelete) {
                this.props.onDelete(node.file);
            } else if (data.action === 'newFile' && this.props.onNewFile) {
                this.props.onNewFile(node.file, "unnamed");
            } else if (data.action === 'newFolder' && this.props.onNewFolder) {
                this.expand(node).then(n => {
                    this.update();
                    this.props.onNewFolder(node.file, "unnamed");
                });
                this.update();
            }
        }
    }

    protected insertNewElement(node: FileNode, createElement: CreateElement) {
        if (node.children) {
            node.children.unshift(new NewElementNode(node, (file, name) => {
                createElement(file, name);
                this.update();
            }));
            this.update();
        }
    }

}
