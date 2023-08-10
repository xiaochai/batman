from PIL import Image, ImageChops, ImageEnhance
import numpy
import tensorflow as tf

# 加载训练好的模型
model = tf.keras.models.load_model('./hand_write_model_with_cnn')
print(model.summary())


def img_pre_process(file, invert=False, bright=1.5):
    # 对图片进行预处理，因为图片是白底黑字，大小不一，所以需要进行一些预处理
    f = Image.open(file)
    # 转成灰度图片，将原来4维(RGBA)转成一维
    i = f.convert("L")
    # 缩放图片
    i = i.resize((28, 28))
    if invert:
        # 转成黑底白字
        i = ImageChops.invert(i)
    if bright > 0:
        # 按需求加亮，加对比度等
        i = ImageEnhance.Brightness(i).enhance(factor=bright)
    # 转化为numpy数组，shape为28*28
    return numpy.array(i)


# 预处理图片，然后转化为ndarray处理
img9 = img_pre_process("./hand_write_sample/9.png", bright=0)
samples = [img9 / 255]
img3 = img_pre_process("./hand_write_sample/3.png", invert=True, bright=0)
samples.append(img3 / 255)
img6 = img_pre_process("./hand_write_sample/6.png", invert=True)
samples.append(img6 / 255)
img6 = img_pre_process("./hand_write_sample/8.png", invert=True)
samples.append(img6 / 255)

# 由于predict的输入是批量的，所以这块的shape为n*28*28，n为样本数量
res = model.predict(numpy.array(samples))
# 结果为对各个样本的概率预测结果，以上结果如下
# [[1.9932010e-12 5.0395615e-10 2.4225795e-09 9.0772168e-08 1.8492664e-06
#   3.9542558e-13 6.0216063e-15 1.3986973e-07 8.4504907e-05 9.9991345e-01]
#  [8.7412150e-10 3.7363580e-07 3.9921666e-04 9.9737668e-01 2.3656726e-08
#   4.9796188e-07 1.3143960e-09 2.2196786e-03 5.4278371e-07 2.8528580e-06]
#  [1.6974575e-08 2.8813119e-11 1.3009218e-09 1.7739095e-11 4.3085522e-09
#   7.8971408e-07 9.9973577e-01 9.4458621e-13 2.6342893e-04 1.8796363e-10]
#  [8.4286776e-18 8.2335221e-14 1.3553557e-11 2.5355894e-14 6.0165419e-17
#   2.1572887e-15 1.5686732e-14 9.7833466e-15 1.0000000e+00 1.2415943e-15]]
print(res)
