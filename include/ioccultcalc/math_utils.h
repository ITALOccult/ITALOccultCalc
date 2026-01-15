#ifndef IOCCULTCALC_MATH_UTILS_H
#define IOCCULTCALC_MATH_UTILS_H

#include <vector>

namespace ioccultcalc {

class MathUtils {
public:
    /**
     * @brief Risolve il sistema lineare Ax = b usando la Decomposizione a Valori Singolari (SVD).
     * @param A Matrice dei coefficienti (m x n)
     * @param b Vettore dei termini noti (m)
     * @param x Vettore delle incognite (n)
     * @param threshold Soglia per azzerare i valori singolari (default 10^-12)
     * @return true se il sistema è risolto, false altrimenti
     */
    static bool solveSVD(const std::vector<std::vector<double>>& A,
                        const std::vector<double>& b,
                        std::vector<double>& x,
                        double threshold = 1e-12);

    /**
     * @brief Esegue la scomposizione in autovettori e autovalori di una matrice simmetrica usando le rotazioni di Jacobi.
     * @param A Matrice simmetrica (n x n) - Verrà sovrascritta dagli autovalori sulla diagonale
     * @param V Matrice degli autovettori (n x n)
     * @param eigenvalues Vettore degli autovalori (n)
     * @return true se converge, false altrimenti
     */
    static bool jacobiEcl(std::vector<std::vector<double>>& A,
                         std::vector<std::vector<double>>& V,
                         std::vector<double>& eigenvalues);

    /**
     * @brief Alias per jacobiEcl (richiesto dal solutore di incertezza).
     */
    static bool eigenDecomposition(const std::vector<std::vector<double>>& A,
                                  std::vector<std::vector<double>>& V,
                                  std::vector<double>& eigenvalues);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_MATH_UTILS_H
