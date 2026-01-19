#include "ioccultcalc/math_utils.h"
#include <Eigen/Dense>
#include <Eigen/SVD>
#include <Eigen/Eigenvalues>

namespace ioccultcalc {

bool MathUtils::solveSVD(const std::vector<std::vector<double>>& A,
                        const std::vector<double>& b,
                        std::vector<double>& x,
                        double threshold) {
    if (A.empty() || b.empty()) return false;

    int rows = A.size();
    int cols = A[0].size();
    
    Eigen::MatrixXd Ae(rows, cols);
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            Ae(i, j) = A[i][j];
        }
    }

    Eigen::VectorXd be(rows);
    for (int i = 0; i < rows; i++) {
        be(i) = b[i];
    }

    // Solve using SVD (JacobiSVD)
    Eigen::VectorXd xe = Ae.jacobiSvd(Eigen::ComputeThinU | Eigen::ComputeThinV).solve(be);
    
    x.resize(cols);
    for (int i = 0; i < cols; i++) {
        x[i] = xe(i);
    }

    return true;
}

bool MathUtils::jacobiEcl(std::vector<std::vector<double>>& A,
                         std::vector<std::vector<double>>& V,
                         std::vector<double>& eigenvalues) {
    if (A.empty()) return false;
    
    int n = A.size();
    Eigen::MatrixXd Ae(n, n);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            Ae(i, j) = A[i][j];
        }
    }

    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> solver(Ae);
    if (solver.info() != Eigen::Success) return false;

    Eigen::VectorXd ev = solver.eigenvalues();
    Eigen::MatrixXd vectors = solver.eigenvectors();

    eigenvalues.resize(n);
    V.assign(n, std::vector<double>(n));
    
    for (int i = 0; i < n; i++) {
        eigenvalues[i] = ev(i);
        for (int j = 0; j < n; j++) {
            V[i][j] = vectors(i, j);
        }
    }

    return true;
}

bool MathUtils::eigenDecomposition(const std::vector<std::vector<double>>& A,
                                  std::vector<std::vector<double>>& V,
                                  std::vector<double>& eigenvalues) {
    std::vector<std::vector<double>> A_copy = A;
    return jacobiEcl(A_copy, V, eigenvalues);
}

} // namespace ioccultcalc
