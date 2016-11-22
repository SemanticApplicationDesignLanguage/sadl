declare module "react-contextmenu" {
    export namespace ContextMenu {
        export interface Props {
            id: string;
        }
    }
    export class ContextMenu extends React.Component<ContextMenu.Props, any> {
    }
    export namespace MenuItem {
        export interface Props<Data> {
            data?: Data;
            onClick?: (event: MouseEvent, data: Data, target: HTMLElement) => void;
        }
    }
    export class MenuItem<Data> extends React.Component<MenuItem.Props<Data>, any> {
    }
    export namespace ContextMenuTrigger {
        export interface Props {
            id: string;
        }
    }
    export class ContextMenuTrigger extends React.Component<ContextMenuTrigger.Props, any> {
    }
}
