import tensorflow as tf
import matplotlib.pyplot as plt
from PIL import Image
import numpy
from keras import layers, models
import datetime

# 加载mnist数据集，如果本地没有，会从远程下载到目录~/.keras/datasets/下
mnist = tf.keras.datasets.mnist
(x_train, y_train), (x_test, y_test) = mnist.load_data()


def plot_samples(samples, labels, rows, cols):
    """
       将样本显示在一张图片上
    """
    fig, axs = plt.subplots(rows, cols, sharex="all", sharey="all")

    axs = axs.flatten()
    for i in range(rows * cols):
        # elem = .reshape(28, 28)
        axs[i].set_title(labels[i], fontsize=10, pad=0)
        # 可以将cmap指定为Greys来展示白底黑字
        axs[i].imshow(samples[i], cmap="gray", interpolation="nearest")
    axs[0].set_xticks([])
    axs[0].set_yticks([])
    plt.tight_layout(h_pad=0.5, pad=0)
    plt.show()


# 60000个训练集和10000个测试集
# (60000, 28, 28) (60000,) (10000, 28, 28) (10000,)
print(x_train.shape, y_train.shape, x_test.shape, y_test.shape)
# 显示部分训练集和测试集
plot_samples(x_train, y_train, 10, 10)
plot_samples(x_test, y_test, 10, 10)
# 可以使用PIL提供的方法来展示单张图片
pil_img = Image.fromarray(numpy.uint8(x_train[1]))
pil_img.show()

# 模型定义，3层卷积(Conv2D)池化(MaxPolling2D)+一层全连接(Dense)
model = models.Sequential()
# 第一层需要指定输出的数据尺寸，后面的层通过找推导不需要指定
model.add(layers.Conv2D(32, (3, 3), activation='relu', input_shape=(28, 28, 1)))
model.add(layers.MaxPooling2D((2, 2)))
# 第2层卷积，卷积核大小为3*3，64个
model.add(layers.Conv2D(64, (3, 3), activation='relu'))
model.add(layers.MaxPooling2D((2, 2)))
# 第3层卷积，卷积核大小为3*3，128个
model.add(layers.Conv2D(128, (3, 3), activation='relu'))
model.add(layers.MaxPooling2D((2, 2)))
model.add(layers.Flatten())
model.add(layers.Dense(64, activation='relu'))
model.add(layers.Dense(10, activation='softmax'))

model.compile(
    optimizer=tf.keras.optimizers.legacy.Adam(0.001),
    loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
    metrics=[tf.keras.metrics.SparseCategoricalAccuracy()],
)

log_dir = "logs/fit/" + datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
tensorboard_callback = tf.keras.callbacks.TensorBoard(log_dir=log_dir, histogram_freq=1)

# 归一化处理
x_train, x_test = x_train / 255.0, x_test / 255.0
# 对样本进行训练，可以指定迭代次数和批量大小
model.fit(x_train, y_train, epochs=10, callbacks=[tensorboard_callback])
# 将模型保存到指定目录，后续可以直接使用
model.save("./hand_write_model_with_cnn")

model.summary()

# 评估模型效果
model.evaluate(x_test, y_test, verbose=2)