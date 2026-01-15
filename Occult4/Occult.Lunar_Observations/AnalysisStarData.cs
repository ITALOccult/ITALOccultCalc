using System;
using System.Text;

namespace Occult.Lunar_Observations
{
	internal class AnalysisStarData
	{
		private const double Radian = 180.0 / Math.PI;

		private double sinPA;

		private double cosPA;

		private double rdt;

		private double t;

		private double r;

		private int certainty;

		private bool peInvolved = true;

		public double SinPA => sinPA;

		public double CosPA => cosPA;

		public double rdT => rdt;

		public double T => t;

		public double Residual => r;

		public int Certainty => certainty;

		public bool PEInvolved => peInvolved;

		public bool ParseReductionLine(string Line)
		{
			if (!double.TryParse(Line.Substring(0, 1), out var result))
			{
				return false;
			}
			if (!double.TryParse(Line.Substring(104, 6), out var result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(Line.Substring(68, 8), out var result3))
			{
				result3 = 0.0;
			}
			r = result3 - result2;
			if (Math.Abs(result3) > 4.0)
			{
				return false;
			}
			if (!int.TryParse(Line.Substring(36, 4), out var result4))
			{
				result4 = 0;
			}
			if (!int.TryParse(Line.Substring(41, 2), out var result5))
			{
				result5 = 1;
			}
			if (!double.TryParse(Line.Substring(44, 2), out var result6))
			{
				result6 = 1.0;
			}
			t = Utilities.BesselianYear(Utilities.JD_from_Date(result4, result5, result6)) - 2000.0;
			if (!double.TryParse(Line.Substring(76, 8), out result))
			{
				result = 0.0;
			}
			sinPA = Math.Sin(result / (180.0 / Math.PI));
			cosPA = Math.Cos(result / (180.0 / Math.PI));
			if (!double.TryParse(Line.Substring(129, 7), out result))
			{
				result = 0.0;
			}
			rdt = result;
			if (!int.TryParse(Line.Substring(64, 1), out certainty))
			{
				certainty = 1;
			}
			peInvolved = "STKX".Contains(Line.Substring(63, 1));
			return true;
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,8:f3}", t + 2000.0);
			stringBuilder.AppendFormat("{0,8:f2}", Residual);
			stringBuilder.AppendFormat("{0,7:f2}", rdT);
			stringBuilder.AppendFormat("{0,10:f5}", SinPA);
			stringBuilder.AppendFormat("{0,9:f5}", CosPA);
			return stringBuilder.ToString();
		}
	}
}
