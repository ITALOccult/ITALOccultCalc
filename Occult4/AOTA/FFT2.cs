using System;

namespace AOTA
{
	public class FFT2
	{
		private class FFTElement
		{
			public double re;

			public double im;

			public FFTElement next;

			public uint revTgt;
		}

		private uint m_logN;

		private uint m_N;

		private FFTElement[] m_X;

		public void init(uint logN)
		{
			m_logN = logN;
			m_N = (uint)(1 << (int)m_logN);
			m_X = new FFTElement[m_N];
			for (uint num = 0u; num < m_N; num++)
			{
				m_X[num] = new FFTElement();
			}
			for (uint num2 = 0u; num2 < m_N - 1; num2++)
			{
				m_X[num2].next = m_X[num2 + 1];
			}
			for (uint num3 = 0u; num3 < m_N; num3++)
			{
				m_X[num3].revTgt = BitReverse(num3, logN);
			}
		}

		public void run(double[] xRe, double[] xIm, bool inverse)
		{
			uint num = m_N >> 1;
			uint num2 = m_N >> 1;
			uint num3 = m_N;
			uint num4 = 1u;
			FFTElement fFTElement = m_X[0];
			uint num5 = 0u;
			double num6 = (inverse ? (1.0 / (double)m_N) : 1.0);
			while (fFTElement != null)
			{
				fFTElement.re = num6 * xRe[num5];
				fFTElement.im = num6 * xIm[num5];
				fFTElement = fFTElement.next;
				num5++;
			}
			for (uint num7 = 0u; num7 < m_logN; num7++)
			{
				double num8 = (double)num4 * 2.0 * Math.PI / (double)m_N;
				if (!inverse)
				{
					num8 *= -1.0;
				}
				double num9 = Math.Cos(num8);
				double num10 = Math.Sin(num8);
				for (uint num11 = 0u; num11 < m_N; num11 += num3)
				{
					FFTElement fFTElement2 = m_X[num11];
					FFTElement fFTElement3 = m_X[num11 + num2];
					double num12 = 1.0;
					double num13 = 0.0;
					for (uint num14 = 0u; num14 < num; num14++)
					{
						double re = fFTElement2.re;
						double im = fFTElement2.im;
						double re2 = fFTElement3.re;
						double im2 = fFTElement3.im;
						fFTElement2.re = re + re2;
						fFTElement2.im = im + im2;
						re2 = re - re2;
						im2 = im - im2;
						fFTElement3.re = re2 * num12 - im2 * num13;
						fFTElement3.im = re2 * num13 + im2 * num12;
						fFTElement2 = fFTElement2.next;
						fFTElement3 = fFTElement3.next;
						double num15 = num12;
						num12 = num12 * num9 - num13 * num10;
						num13 = num15 * num10 + num13 * num9;
					}
				}
				num >>= 1;
				num2 >>= 1;
				num3 >>= 1;
				num4 <<= 1;
			}
			for (fFTElement = m_X[0]; fFTElement != null; fFTElement = fFTElement.next)
			{
				uint revTgt = fFTElement.revTgt;
				xRe[revTgt] = fFTElement.re;
				xIm[revTgt] = fFTElement.im;
			}
		}

		private uint BitReverse(uint x, uint numBits)
		{
			uint num = 0u;
			for (uint num2 = 0u; num2 < numBits; num2++)
			{
				num <<= 1;
				num |= x & 1u;
				x >>= 1;
			}
			return num;
		}
	}
}
