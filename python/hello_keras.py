import os
os.environ["KERAS_BACKEND"] = "jax"
import keras
print(keras.__version__)