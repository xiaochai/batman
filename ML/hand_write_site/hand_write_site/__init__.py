import tensorflow as tf

print("loading model...")
model = tf.keras.models.load_model('../hand_write_model_with_cnn')
print("load model finished.")
