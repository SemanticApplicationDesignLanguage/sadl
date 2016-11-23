declare module 'react-treebeard' {
    export interface TreeNode {
        id?: string;
        name: string;
        children?: TreeNode[];
        toggled?: boolean;
        active?: boolean;
        loading?: boolean;
    }
    export namespace Container {
        export interface Props {
            node: TreeNode
        }
    }
    export class Container extends React.Component<Container.Props, {}> {
    }
    export interface TreeNodeDecorators {
        Loading: any
        Toggle: any
        Header: any
        Container: typeof Container
    }
    export namespace TreeBeard {
        export interface Props {
            data: TreeNode | TreeNode[];
            onToggle?: (node: TreeNode, toggled: boolean) => void;
            decorators?: TreeNodeDecorators;
            style: any;
        }
    }
    export class Treebeard extends React.Component<TreeBeard.Props, {}> {
    }
    export var decorators: TreeNodeDecorators
}