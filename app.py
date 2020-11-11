import numpy as np
from flask import Flask, request, jsonify, render_template
import pickle
import requests
import pandas as pd
#import requests
import flask
app = flask.Flask(__name__)

model = pickle.load(open('model.pkl', 'rb'))

# Set up the main route
@app.route('/', methods=['GET', 'POST'])
def predict():
    
    if flask.request.method == 'GET':
        # Just render the initial form, to get input
        return(flask.render_template('index.html'))
    
    if flask.request.method == 'POST':
         PROD_CD=flask.request.form['PROD_CD']
         SLSMAN_CD=flask.request.form['SLSMAN_CD']
         PLAN_MONTH=flask.request.form['PLAN_MONTH']
         #PLAN_YEAR=flask.request.form['PLAN_YEAR']
         TARGET_IN_EA=flask.request.form['TARGET_IN_EA']
        # Extract the input

        # Make DataFrame for model
        
         input_variables=pd.DataFrame([[PROD_CD,SLSMAN_CD,PLAN_MONTH,TARGET_IN_EA]],columns=['PROD_CD','SLSMAN_CD','PLAN_MONTH','TARGET_IN_EA'],dtype=float,index=['input'])
   

        # Get the model's prediction
         prediction = model.predict(input_variables)[0]
    
        # Render the form again, but add in the prediction and remind user
        # of the values they input before
         return flask.render_template('index.html',original_input={'PROD_CD':PROD_CD,'SLSMAN_CD':SLSMAN_CD,'MONTH':PLAN_MONTH,'TARGET':TARGET_IN_EA},result=prediction,)

if __name__ == "__main__":
    app.run(debug=True)