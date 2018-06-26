class Solution(object):
    def largestOverlap(self, A, B):
        self.init(A, B)
        return max(
            self.compare(x_trans, y_trans)
            for x_trans in range(-(self.row_length-1), self.row_length)
            for y_trans in range(-(self.column_count-1), self.column_count)
        )

    def init(self, A, B):
        self.A = A
        self.B = B
        self.row_length = len(A[0])
        self.column_count = len(A)

    def compare(self, x_trans, y_trans):
        overlap_count = 0
        for row_selection in range(
                max(y_trans, 0),
                min(self.column_count, self.column_count + y_trans)
        ):
            for column_selection in range(
                    max(x_trans, 0),
                    min(self.row_length, self.row_length + x_trans)
            ):
                if (
                        self.A[row_selection][column_selection] ==
                        self.B[row_selection - y_trans][column_selection - x_trans] == 1
                ):
                    overlap_count += 1
        return overlap_count

if __name__ == '__main__':
    sol = Solution()
    sol.init([[1,1,0],[0,1,0],[0,1,0]],
             [[1,0,0],[0,1,1],[0,0,1]])
    print(sol.compare(-1, -1))
