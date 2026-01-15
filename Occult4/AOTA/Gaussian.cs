using System;

namespace AOTA
{
	public class Gaussian
	{
		private static bool uselast = true;

		private static double next_gaussian = 0.0;

		private static Random random = new Random();

		private static double ThisInst = 0.0;

		internal static double MultipleOfSD = 2.0;

		public static double BoxMuller(double mean, double standard_deviation)
		{
			do
			{
				ThisInst = BoxMuller();
			}
			while (Math.Abs(ThisInst) > MultipleOfSD);
			return mean + ThisInst * standard_deviation;
		}

		public static double BoxMuller()
		{
			if (uselast)
			{
				uselast = false;
				return next_gaussian;
			}
			double num;
			double num2;
			double num3;
			do
			{
				num = 2.0 * random.NextDouble() - 1.0;
				num2 = 2.0 * random.NextDouble() - 1.0;
				num3 = num * num + num2 * num2;
			}
			while (num3 >= 1.0 || num3 == 0.0);
			num3 = Math.Sqrt(-2.0 * Math.Log(num3) / num3);
			next_gaussian = num2 * num3;
			uselast = true;
			return num * num3;
		}

		public static void SetMaximumSD_Multiple(double SDMultiple)
		{
			MultipleOfSD = SDMultiple;
		}
	}
}
