import * as React from 'react';
import { Treebeard, TreeNode } from 'react-treebeard';

import {
    File
} from '../workspace';

export class FileNode implements TreeNode {

    readonly file: File
    readonly name: string

    readonly parent?: FileNode
    readonly children?: FileNode[]

    toggled?: boolean
    loading?: boolean

    constructor(file: File, parent?: FileNode) {
        this.file = file;
        this.parent = parent;
        this.name = this.getName(file);
        if (file.directory) {
            this.toggled = !!file.children;
            this.children = this.getChildren(file);
        }
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

export interface IExplorerProps {
    readonly onDidMount?: (explorer: Explorer) => void;
    readonly onWillUnmount?: (explorer: Explorer) => void;
    readonly onOpen?: (file: File) => void;
    readonly onExpand?: (file: File) => PromiseLike<File | null>;
}

export interface IExplorerState {
    rootNode?: TreeNode | FileNode
    selectedNode?: TreeNode
}

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
        return <Treebeard
            data={this.state.rootNode!}
            onToggle={(node, toggled) => this.onToggle(node, toggled)}
            />;
    }

    open(file: File) {
        this.setState({
            rootNode: new FileNode(file)
        });
    }

    update() {
        this.setState({});
    }

    merge(node: FileNode, file: File) {
        const parent = node.parent;
        if (parent && parent.children) {
            const index = parent.children.indexOf(node);
            parent.children[index] = new FileNode(file, parent);
            this.update();
        }
    }

    expand(node: FileNode) {
        if (!node.file.children && this.props.onExpand) {
            node.loading = true;
            this.props.onExpand(node.file).then(file => {
                node.loading = false;
                if (file) {
                    this.merge(node, file);
                }
            });
        }
    }

    onToggle(node: TreeNode, toggled: boolean): void {
        if (this.state.selectedNode) {
            this.state.selectedNode.active = false;
        }
        node.active = true;

        if (node instanceof FileNode) {
            if (node.file.directory) {
                node.toggled = toggled;
                if (toggled) {
                    this.expand(node);
                }
            } else if (this.props.onOpen) {
                this.props.onOpen(node.file);
            }
        }

        this.setState({
            selectedNode: node
        });
    }

}
