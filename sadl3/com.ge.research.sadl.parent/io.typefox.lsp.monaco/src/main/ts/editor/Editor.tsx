import * as React from 'react';
import * as ReactDOM from 'react-dom';

export interface EditorProps {
    onEditorDidMount?: (editor: monaco.editor.IStandaloneCodeEditor) => void;
    onEditorWillUnmount?: (editor: monaco.editor.IStandaloneCodeEditor) => void;
}

export class Editor extends React.Component<EditorProps, {}> {

    protected editor: monaco.editor.IStandaloneCodeEditor

    render() {
        const style = {
            height: '600px'
        };
        return <div style={style} />;
    }

    componentDidMount(): void {
        const element = ReactDOM.findDOMNode<HTMLElement>(this);
        this.editor = monaco.editor.create(element);
        if (this.props.onEditorDidMount) {
            this.props.onEditorDidMount(this.editor);
        }
    }

    componentWillUnmount(): void {
        if (this.props.onEditorWillUnmount) {
            this.props.onEditorWillUnmount(this.editor);
        }
        this.editor.dispose();
    }

}
