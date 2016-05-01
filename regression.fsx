#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"
#load "../packages/MathNet.Numerics.FSharp.3.11.1/MathNet.Numerics.fsx"
open MathNet.Numerics
open FSharp.Data

type Homedata = CsvProvider< @"C:\home_data.csv", Schema="int64,string,float,float,float,float,float,float,float,float,float,float,float,float,float,float,int,float,float,float,float" >
let houses = Homedata.Load(@"C:\home_data.csv")

// What is the average house price for zip code 98039?
let avgPrice98039 = 
    houses.Rows
    |> Seq.filter (fun row -> row.Zipcode = 98039)
    |> Seq.map (fun row -> row.Price)
    |> Seq.average 

// What fraction of the houses have living space between 2000 sq.ft. and 4000 sq.ft.?
let countAll = float (Seq.length houses.Rows)
let count2to4 =
    houses.Rows
    |> Seq.filter (fun row -> row.Sqft_living > 2000. && row.Sqft_living <= 4000.)
    |> Seq.length
    |> float
let fraction = count2to4 / countAll

// Building a regression model with two different features sets and comparing their RMSE (root mean squared error) values
let x1 = houses.Rows |> Seq.map (fun row -> [| row.Bedrooms; row.Bathrooms; row.Sqft_living; row.Sqft_lot; row.Floors; float row.Zipcode |]) |> Seq.toArray
let x2 = houses.Rows |> Seq.map (fun row -> [| row.Bedrooms; row.Bathrooms; row.Sqft_living; row.Sqft_lot; row.Floors; float row.Zipcode; row.Condition; row.Grade; row.Waterfront;  
    row.View; row.Sqft_above; row.Sqft_basement; row.Yr_built; row.Yr_renovated; row.Lat; row.Long; row.Sqft_living15; row.Sqft_lot15 |]) |> Seq.toArray
let y = houses.Rows |> Seq.map (fun row -> row.Price) |> Seq.toArray
let p1 = Fit.multiDim true x1 y
let p2 = Fit.multiDim true x2 y
let y_hat1 = 
    houses.Rows 
    |> Seq.map (fun row -> p1.[0] + p1.[1]*row.Bedrooms + p1.[2]*row.Bathrooms + p1.[3]*row.Sqft_living + p1.[4]*row.Sqft_lot + p1.[5]*row.Floors + p1.[6]*(float row.Zipcode)) 
    |> Seq.toArray
let RMSE1 = sqrt(Distance.MSE(y, y_hat1))
let y_hat2 = 
    houses.Rows 
    |> Seq.map (fun row -> p2.[0] + p2.[1]*row.Bedrooms + p2.[2]*row.Bathrooms + p2.[3]*row.Sqft_living + p2.[4]*row.Sqft_lot + p2.[5]*row.Floors + p2.[6]*(float row.Zipcode)
                            + p2.[7]*row.Condition + p2.[8]*row.Grade + p2.[9]*row.Waterfront + p2.[10]*row.View + p2.[11]*row.Sqft_above + p2.[12]*row.Sqft_basement + p2.[13]*row.Yr_built + p2.[14]*row.Yr_renovated
                            + p2.[15]*row.Lat + p2.[16]*row.Long + p2.[17]*row.Sqft_living15 + p2.[18]*row.Sqft_lot15)
    |> Seq.toArray
let RMSE2 = sqrt(Distance.MSE(y, y_hat2))
RMSE1 - RMSE2
