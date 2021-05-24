# papermapOnWebmap

画像コンテンツを地図上に直接貼り付ける方法を考える。

https://mghs15.github.io/papermapOnWebmap/

## データ加工
* 地図空中写真閲覧サービスから、画像コンテンツと使えそうなデータを持ってくる。

* 画像から余計な部分をトリミングする。
→`trimming.R`
※とりあえず、戦前の地図データ用に調整。地図の装飾に合わせて調整が必要。

* 画像位置を表示するGeoJSONを作る。
→`process_searchdata2geojson.js`


## 出典
### 画像コンテンツ・付属情報
* 地図・空中写真閲覧サービス https://mapps.gsi.go.jp/

### 背景地図
* 地理院地図Vector https://maps.gsi.go.jp/vector/

### 参考資料
* https://qiita.com/yoya/items/96c36b069e74398796f3


