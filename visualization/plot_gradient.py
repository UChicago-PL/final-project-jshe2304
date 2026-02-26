"""Plot a 3D surface and 2D gradient field for f(x, y) = sin(x) * cos(y)."""

import csv
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401
import matplotlib

matplotlib.rcParams.update({"font.size": 11})


def main():
    path = "visualization/data/gradient_field.csv"
    xs, ys, fs, dxs, dys = [], [], [], [], []
    with open(path) as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            xs.append(float(row["x"]))
            ys.append(float(row["y"]))
            fs.append(float(row["f"]))
            dxs.append(float(row["df_dx"]))
            dys.append(float(row["df_dy"]))

    n = int(np.sqrt(len(xs)))
    X = np.array(xs).reshape(n, n)
    Y = np.array(ys).reshape(n, n)
    F = np.array(fs).reshape(n, n)
    DX = np.array(dxs).reshape(n, n)
    DY = np.array(dys).reshape(n, n)

    fig = plt.figure(figsize=(14, 5.5))

    # Left: 3D surface
    ax1 = fig.add_subplot(1, 2, 1, projection="3d")
    ax1.plot_surface(X, Y, F, cmap="viridis", edgecolor="none", alpha=0.9)
    ax1.set_xlabel("x")
    ax1.set_ylabel("y")
    ax1.set_zlabel("f(x, y)")
    ax1.set_title("f(x, y) = sin(x) cos(y)")

    # Right: contour + gradient quiver
    ax2 = fig.add_subplot(1, 2, 2)
    cf = ax2.contourf(X, Y, F, levels=30, cmap="viridis")
    fig.colorbar(cf, ax=ax2, label="f(x, y)")
    # Subsample arrows for readability
    step = max(1, n // 15)
    ax2.quiver(
        X[::step, ::step],
        Y[::step, ::step],
        DX[::step, ::step],
        DY[::step, ::step],
        color="white",
        alpha=0.8,
    )
    ax2.set_xlabel("x")
    ax2.set_ylabel("y")
    ax2.set_title("Gradient Field ∇f")
    ax2.set_aspect("equal")

    plt.tight_layout()
    out = "visualization/images/gradient_field.png"
    fig.savefig(out, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved {out}")


if __name__ == "__main__":
    main()
