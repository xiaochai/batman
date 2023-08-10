from django.http import HttpResponse
from django.shortcuts import render

from PIL import Image, ImageChops
import numpy, io, json, base64
from . import model


def index(request):
    return render(request, 'index.html', {})


def submit(request):
    # canvas图片的数据会转化为base64通过post的data传过来，但这块会有一些前置的信息，需要去除
    base64_str = request.POST['data'].split(",")[1]
    # print(base64_str)

    # base64的图片信息转化为Image
    raw = Image.open(io.BytesIO(base64.decodebytes(bytes(base64_str, "utf-8"))))
    # 由于传过来的image是个PNG，背景是透明的，所以不能直接转灰度，需要将其copy到一张带白底的图上
    img = Image.new('RGB', raw.size, (255, 255, 255))
    img.paste(raw, (0, 0), raw)
    # 然后对图片进行预处理
    img = img.convert("L")
    img = img.resize((28, 28))
    img = ImageChops.invert(img)
    # img.show()

    # 模型预测
    predict = model.predict(numpy.array([numpy.array(img) / 255]))
    # 将处理完的图片转成base64返回
    output_buffer = io.BytesIO()
    img.save(output_buffer, format='JPEG')
    base64_data = str(base64.b64encode(output_buffer.getvalue()), 'utf-8')
    # 同时返回预测结果和处理后的图片
    return HttpResponse(json.dumps(
        (predict.tolist(), base64_data)
    ))
