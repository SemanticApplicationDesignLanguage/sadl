declare module 'react-treebeard' {
    export interface TreeNode {
        id?: string;
        name: string;
        children?: TreeNode[];
        toggled?: boolean;
        active?: boolean;
        loading?: boolean;
    }
    export interface TreeBeardProps {
        data: TreeNode | TreeNode[],
        onToggle?: (node: TreeNode, toggled: boolean) => void;
    }
    export class Treebeard extends React.Component<TreeBeardProps, {}> {
    }
}