import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

# Define your control points
P0 = np.array([1, 0, 0])
P1 = np.array([1, 1, 1])
P2 = np.array([2, 1, 0])

# Compute the Bézier curve
t = np.linspace(0, 1, 50)[:, np.newaxis]
curve = (1 - t)**2 * P0 + 2 * (1 - t) * t * P1 + t**2 * P2

# Calculate difference vectors
V1 = P1 - P0
V2 = P2 - P0

# Create the figure and axis objects
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# Plot the control points
ax.scatter(*P0, marker='o', label='$P_0^*$', c='r', s=50)
ax.scatter(*P1, marker='o', label='$P_1^*$', c='g', s=50)
ax.scatter(*P2, marker='o', label='$P_2^*$', c='b', s=50)
ax.scatter(0, 0, 0, marker='s', label='Origin', c='black', s=50)

# Annotate the points
for point, label in zip([P0, P1, P2], ['$P_0^*$', '$P_1^*$', '$P_2^*$']):
    ax.text(*point, label, fontsize=18)

# Plot vectors from origin to P0, P1, and P2
for point in [P0, P1, P2]:
    ax.quiver(0, 0, 0, point[0], point[1], point[2], arrow_length_ratio=0.1, color='black')

# Plot difference vectors V1 and V2
ax.quiver(*P0, *V1, arrow_length_ratio=0.1, color='green', label='$V1=P1-P0$')
ax.quiver(*P0, *V2, arrow_length_ratio=0.1, color='green', label='$V2=P2-P0$')

# Plot the Bézier curve
ax.plot(curve[:, 0], curve[:, 1], curve[:, 2], linewidth=2, color='magenta')

# Shade the area spanned by P0, P1, P2
polygon = Poly3DCollection([[P0, P1, P2]], alpha=0.25, color='cyan')
ax.add_collection3d(polygon)

# Set axis labels
ax.set_xlabel('$w_1$', fontsize=18)
ax.set_ylabel('$w_2$', fontsize=18)
ax.set_zlabel('$w_3$', fontsize=18)


# Modify tick marks
ax.set_xticks([0, 1, 2])
ax.set_yticks([0, 1])
ax.set_zticks([0, 1])

# Increase tick mark font size by 50%
ax.tick_params(axis='both', which='major', labelsize=15)

# Save the figure as a PDF file
plt.savefig("sub_space.pdf")

# Show the plot
plt.show()