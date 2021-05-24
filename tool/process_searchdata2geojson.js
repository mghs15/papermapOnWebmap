
const fs = require('fs')
const child_process = require('child_process');

const app = (filename) => {
    const data = fs.readFileSync(filename, 'utf8');
    
    if(!data){
      console.log("Error:", filename);
      return;
    }
    
    // BOMを無視する https://webbibouroku.com/Blog/Article/node-bom-utf8
    if (data.charCodeAt(0) === 0xFEFF) {
      json = data.substr(1);
    }else{
      json = data;
    }
    
    const resjson = JSON.parse(json);
    const res = resjson.resultJsondata;
    
    const geojson = {
      "type": "FeatureCollection",
      "features": []
    }
    
    res.forEach( f => {
      
      //集約
      const ndjson = {
        "type": "Feature",
        "properties": {
          "specificationId": f.specificationId,
          "searchDateDisp": +f.searchDateDisp,
          "bbox": [
  				[f.imageLeftTopLon, f.imageLeftTopLat],
  				[f.imageRightBottomLon, f.imageRightTopLat],
  				[f.imageRightTopLon, f.imageRightBottomLat],
  				[f.imageLeftBottomLon, f.imageLeftBottomLat]
          ]
        },
        "geometry": {
          "type": "Point",
          "coordinates": [
            f.imageCenterLon + (Math.random() - 0.5)/100,
            f.imageCenterLat + (Math.random() - 0.5)/100,
          ]
        }
      }
      
      geojson.features.push(ndjson);
      
    });
    
    const outfile = "out_" + filename + ".geojson";
    const str = JSON.stringify(geojson);
    try{
        fs.appendFileSync(outfile, str);
    }catch(err){
        console.log(err);
    }
} 

app("WestGunma.json");


