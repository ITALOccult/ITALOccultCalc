using System;
using System.IO;

namespace Occult
{
	public static class Phoebe
	{
		private static bool? Phoebe_DataExists = null;

		private static double JD_First = 0.0;

		private static double JD_Last = 0.0;

		private static double JD_StartBlock = 0.0;

		private static double JD_EndBlock = 0.0;

		private static double[] cX = new double[7];

		private static double[] cY = new double[7];

		private static double[] cZ = new double[7];

		private static double[] P = new double[7];

		public static void Phoebe_Init()
		{
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\SaturnIX.bin"))
			{
				Phoebe_DataExists = false;
				return;
			}
			using FileStream input = new FileStream(Utilities.AppPath + "\\Resource Files\\SaturnIX.bin", FileMode.Open, FileAccess.Read);
			using BinaryReader binaryReader = new BinaryReader(input);
			JD_First = binaryReader.ReadDouble();
			JD_Last = binaryReader.ReadDouble();
			Phoebe_DataExists = true;
			JD_StartBlock = (JD_EndBlock = 0.0);
		}

		public static bool Phoebe_Chebychev(double JD, out double x2000, out double y2000, out double z2000)
		{
			double num = 0.0;
			int num2 = 7;
			long num3 = 0L;
			x2000 = (y2000 = (z2000 = 0.0));
			if (!Phoebe_DataExists.HasValue)
			{
				Phoebe_Init();
			}
			if (Phoebe_DataExists == false)
			{
				return false;
			}
			if ((JD < JD_First) | (JD >= JD_Last))
			{
				return false;
			}
			num3 = (int)Math.Floor((JD - JD_First) / 35.0) * 100 + 16;
			num = (JD - JD_First) % 35.0 / 35.0;
			if ((JD < JD_StartBlock) | (JD > JD_EndBlock))
			{
				using FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\SaturnIX.bin", FileMode.Open, FileAccess.Read);
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				fileStream.Seek(num3, SeekOrigin.Begin);
				JD_StartBlock = binaryReader.ReadDouble();
				JD_EndBlock = binaryReader.ReadDouble();
				for (int i = 0; i < num2; i++)
				{
					cX[i] = binaryReader.ReadSingle();
				}
				for (int j = 0; j < num2; j++)
				{
					cY[j] = binaryReader.ReadSingle();
				}
				for (int k = 0; k < num2; k++)
				{
					cZ[k] = binaryReader.ReadSingle();
				}
			}
			double num4 = 2.0 * num - 1.0;
			double num5 = 2.0 * num4;
			P[0] = 1.0;
			P[1] = num4;
			for (int l = 2; l < num2; l++)
			{
				P[l] = num5 * P[l - 1] - P[l - 2];
			}
			P[0] = 0.5;
			for (int m = 0; m < num2; m++)
			{
				x2000 += cX[m] * P[m];
			}
			for (int n = 0; n < num2; n++)
			{
				y2000 += cY[n] * P[n];
			}
			for (int num6 = 0; num6 < num2; num6++)
			{
				z2000 += cZ[num6] * P[num6];
			}
			return true;
		}
	}
}
