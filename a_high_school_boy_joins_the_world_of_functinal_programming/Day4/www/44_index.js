var Component = React.createClass(
{
    getInitialState: function()
    { // 表計算アプリのマス目に相当する
        return {
            a: undefined,
            b: undefined
        };
    },
    handleChange: function(event)
    {
        var a = event.target.value * 1; // a のマス目の値
        this.setState({b: a * 5});      // b のマス目の値は、a のマス目の値の 5 倍と等しい
    },
    render: function()
    { // a のマス目の値を監視すると同時に
        return <div>
            <input type = "text" onChange = {this.handleChange} />
            <input type = "text" value = {this.state.b} />
        </div>;
    } // b のマス目はリアクティブに再計算され
});

var react1 = React.render( <Component/>, document.body);
// 宣言する。リアクティブに自動的にスクリーンに再描画される！！
