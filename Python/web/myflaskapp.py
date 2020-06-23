from flask import Flask, request, render_template


app = Flask(__name__)
@app.route('/')
def index():
    return "<span style='color:red'>I am app 1</span>"

@app.route('/page', methods=['POST', 'GET'])
def page():
    return "GET:{}\nMETHOD:{}\nPOST:{}\n".format(request.args, request.method, request.form)

@app.route('/hello/<name>')
def hello(name=None):
    return render_template('hello.html', name=name)