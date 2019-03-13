import * as React from "react";
import "react-table/react-table.css";
import { ProcessView } from "./ProcessView";
import { haskellAPI } from "../middleware/haskell-api";
import { EdgesView } from "./EdgesView";
import { GraphView } from "./GraphView.js";
import { EdgeJSON } from "./EdgeJSON";

interface NodeViewProps {
    onNIdChange: any;
    selectedNId: any;
    synthesisStatus?: any;
}

interface NodeViewState {
    testBenchDump: any;
    synthesisNode: any;
    selectedNId: any;
    synthesisStatus: any;
    view: any;
    model: any;
    scOptions: any;
}

export class NodeView extends React.Component<NodeViewProps, NodeViewState> {

    onNIdChange: any;

    constructor (props: NodeViewProps) {
        super(props);
        this.onNIdChange = props.onNIdChange;
        this.state = {
            testBenchDump: null,
            synthesisNode: null,
            selectedNId: props.selectedNId,
            synthesisStatus: props.synthesisStatus,
            view: " update" ,
            model: null,
            scOptions: null
        };
        this.handleViewChange(props.selectedNId, props.synthesisStatus, " synthesisNode" );
    }

    handleViewChange (nid: any, synthesisStatus: any, view: any) {
        console.debug(" NodeView:handleViewChange(" , nid, view, " ) // this.state.view:" , this.state.view);
        if (nid === undefined || nid === null) return;

        this.setState({selectedNId: nid, synthesisStatus: synthesisStatus, view: " update" });
        if (view === " process" ) this.updateModel(nid, " process" );
        if (view === " synthesisNode" ) this.updateModel(nid, " synthesisNode" );
        if (view === " testbench" ) this.updateTestBench(nid, " testbench" );
        if (view === " edges" ) this.setState({view: " edges" });
    }

    componentWillReceiveProps (props) {
        console.debug(" NodeView:componentWillReceiveProps(" , props, " )" );
        let view = this.state.view;
        if (view === " update" ) view = " synthesisNode";
        if (this.state.selectedNId !== props.selectedNId) {
            this.handleViewChange(props.selectedNId, props.synthesisStatus, view);
        }
    }

    allBestThread (nid: any, n: any) {
        if (nid === undefined || nid === null) return;
        console.debug(" NodeView:allBestThread(" , nid, n, " )" );
        haskellAPI.allBestThread(nid, n)
        .then((response: any) => {
            let newNid = response.data;
            this.onNIdChange(newNid);
        })
        .catch((err: any) => alert(err));
    }

    obviousBindThread (nid: any) {
        if (nid === undefined || nid === null) return;
        console.debug(" NodeView:obviousBindThread(" , nid, " )" );
        haskellAPI.obviousBindThread(nid)
        .then((response: any) => {
            let newNid = response.data;
            this.onNIdChange(newNid);
        })
        .catch((err: any) => alert(err));
    }

    simpleSynthesis (nid: any, deep?: any) {
        if (nid === undefined || nid === null) return;
        console.debug(" NodeView:simpleSynthesis(" , nid, " )" );
        // FIXME: simpleSynthesis(nid, deep) Expected 1 arguments, but got 2
        haskellAPI.simpleSynthesis(nid)
        .then((response: any) => {
            let newNid = response.data;
            this.onNIdChange(newNid);
        })
        .catch((err: any) => alert(err));
    }

    smartBindSynthesisIO (nid: any, deep?: any) {
        if (nid === undefined || nid === null) return;
        console.debug(" NodeView:simpleSynthesis(" , nid, " )" );
        // FIXME: smartBindSynthesisIO(nid, deep) Expected 1 arguments, but got 2
        haskellAPI.smartBindSynthesisIO(nid)
        .then((response: any) => {
            let newNid = response.data;
            this.onNIdChange(newNid);
        })
        .catch((err: any) => alert(err));
    }

    updateModel (nid: any, view: any) {
        haskellAPI.getNode(nid)
        .then((response: any) => {
            this.setState({
                synthesisNode: response.data,
                view: view
            });
        })
        .catch((err: any) => console.log(err));
    }

    updateTestBench (nid: any, view: any) {
        haskellAPI.runTestBench(nid, " web_ui" )
        .then((response: any) => {
            this.setState({
                testBenchDump: response.data,
                view: view
            });
        })
        .catch((err: any) => {
            alert(" Can not take testbench, maybe, because it is not completed synthesis!\n"  + err);
            this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, " synthesisNode" );
        });
    }

    render () {
        return (
        <div>
            { this.state.selectedNId === null && <pre> synthesis is not selected </pre> }

            { this.state.selectedNId !== null &&
            <div>
                <div className=" tiny button-group" >
                    <a className=" button primary"  onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, " synthesisNode" )}>synthesis node</a>
                    <a className=" button primary"  onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, " process" )}>process</a>
                    <a className=" button primary"  onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, " edges" )}>edges</a>
                    <a className=" button primary"  onClick={() => this.handleViewChange(this.state.selectedNId, this.state.synthesisStatus, " testbench" )}>testbench</a>
                </div>
                <div className=" tiny button-group" >
                    <a className=" button primary"  onClick={() => this.simpleSynthesis(this.state.selectedNId)}>simple synthesis</a>
                    <a className=" button primary"  onClick={() => this.smartBindSynthesisIO(this.state.selectedNId)}>smart bind synthesis</a>
                </div>
                <div className=" tiny button-group" >
                    <a className=" button primary"  onClick={() => this.obviousBindThread(this.state.selectedNId)}>oblious bind thread</a>
                    <a className=" button primary"  onClick={() => this.allBestThread(this.state.selectedNId, 0)}>best thread</a>
                    <a className=" button primary"  onClick={() => this.allBestThread(this.state.selectedNId, 1)}>all best thread 1</a>
                    <a className=" button primary"  onClick={() => this.allBestThread(this.state.selectedNId, 2)}>all best thread 2</a>
                </div>
                { this.state.view === " update"  && <pre> updating... </pre> }
                { this.state.view === " synthesisNode"  &&
                <div>
                    <div className="edgeGraphContainer" style={{'display': "inline-block"}}>
                        <GraphView 
                        view = { this.state.view }
                        selectedNId = { this.state.selectedNId }
                        />  
                    </div> 
                    <div className="jsonContainer" style={{'display': "inline-block", 'vertical-align': 'top', 'width': '270px'}}>
                        <EdgeJSON edge={ this.state.synthesisNode.nModel.processor} label={"nModel.processor"}  />
                    </div> 
                    <div className="jsonContainer" style={{'display': "inline-block", 'vertical-align': 'top'}}>
                        <EdgeJSON edge={ this.state.synthesisNode.nModel.dfg} label={"nModel.dfg"}  />
                    </div> 
                    <div className="jsonContainer" style={{'display': "inline-block", 'vertical-align': 'top'}}>
                        <div  >
                            <EdgeJSON edge={ this.state.synthesisNode.nId} label={"nId"}  />
                        </div> 
                        <br></br>
                        <div  >
                            <EdgeJSON edge={ this.state.synthesisNode.nIsComplete} label={"nIsComplete"}  />
                        </div> 
                    </div>
                </div>
                }
                 {/* this.state.synthesisNode.nModel.processor
                 this.state.synthesisNode.nModel.dfg 
                 this.state.synthesisNode.nId
                 this.state.synthesisNode.nIsComplete */}
            
                { this.state.view === " process"  &&
                    <ProcessView
                        steps={this.state.synthesisNode.nModel.processor.process.steps}
                        relations={this.state.synthesisNode.nModel.processor.process.relations}
                    />
                }
                { this.state.view === " edges"  &&
                    <EdgesView
                        selectedNId={ this.state.selectedNId }
                        onNIdChange={ (nid: any) => this.onNIdChange(nid)}
                    />
                }
                { this.state.view === " testbench"  &&
                    <div>
                        Status: <pre> { JSON.stringify(this.state.testBenchDump.tbStatus) } </pre>
                        <hr />
                        Compiler output:
                        <pre> { this.state.testBenchDump.tbCompilerDump } </pre>
                        <hr />
                        Simulation output:
                        <pre> { this.state.testBenchDump.tbSimulationDump } </pre>
                        <hr />
                        <pre> { JSON.stringify(this.state.testBenchDump, null, 2) } </pre>
                    </div>
                }
            </div>
            }
        </div>
        );
    }
}
