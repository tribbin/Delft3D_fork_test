class Comparison2DArrayResult:
    """A class to store the results of comparing two 2D arrays."""

    def __init__(self) -> None:
        self.max_abs_diff = 0.0
        self.max_abs_diff_coordinates = (0, 0)
        self.max_abs_diff_values = (0.0, 0.0)  # Left and right.
        self.min_ref_value = 0.0
        self.max_ref_value = 0.0
        self.column_id = 0
        self.row_id = 0
