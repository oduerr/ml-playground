import tensorflow as tf

# Define your model
model = tf.keras.Sequential([
    tf.keras.layers.Dense(10, activation='relu', name='first_layer', input_shape=(5,)),
    tf.keras.layers.Dense(1, name='output_layer')
])

# Build the model
model.build(input_shape=(None, 5))

# Compile the model (optional, if you plan to use high-level training APIs)
model.compile(optimizer='adam', loss='mse')

# Print model summary to verify the layers and their shapes
model.summary()

# Define input and target
x = tf.random.normal((1, 5))
y = tf.random.normal((1, 1))

# Select the variables you want to compute second-order derivatives for
selected_vars = model.layers[0].trainable_variables

# Verify that selected_vars is not empty
print("Selected variables:", selected_vars)

if not selected_vars:
    raise ValueError("No trainable variables found in the selected layer.")

with tf.GradientTape(persistent=True) as tape2:
    with tf.GradientTape(persistent=True) as tape1:
        # We don't need to explicitly watch variables, as trainable variables are watched by default
        
        # Forward pass
        y_pred = model(x)
        loss = tf.reduce_mean(tf.square(y - y_pred))
    
    # Compute first-order gradients
    grads = tape1.gradient(loss, selected_vars)

# Compute second-order derivatives (Hessian)
hessians = [tape2.jacobian(g, selected_vars) for g in grads]

# Print results
for i, (var, grad, hess) in enumerate(zip(selected_vars, grads, hessians)):
    print(f"Variable {i}:")
    print(f"Name: {var.name}")
    print(f"Shape: {var.shape}")
    print(f"Gradient shape: {grad.shape}")
    print(f"Hessian shape: {[h.shape for h in hess]}")
    print()

# Clean up
del tape1
del tape2