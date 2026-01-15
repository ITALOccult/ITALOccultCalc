using System;
using System.Collections.Generic;

namespace Occult.Mapping
{
	internal class LOLA_HiRes
	{
		private const double Radian = 180.0 / Math.PI;

		internal static bool GetNewStartingPoint(List<Points> Limb, int StartLocation, out int NextLocation)
		{
			double num = -9.0;
			if (StartLocation >= Limb.Count)
			{
				StartLocation = 0;
			}
			double num2 = Limb[StartLocation].AA + 0.04;
			double num3 = 0.0;
			int num4 = StartLocation + 1;
			NextLocation = 0;
			do
			{
				if (num4 >= Limb.Count)
				{
					num4 = 0;
					num3 = 360.0;
				}
				if (Limb[num4].Valid)
				{
					if (Limb[num4].AA + num3 > num2)
					{
						break;
					}
					if (Limb[num4].H > num)
					{
						num = Limb[num4].H;
						NextLocation = num4;
					}
				}
				num4++;
			}
			while (num4 < Limb.Count);
			return num > 0.0;
		}

		internal static bool GetNextWithSimilarD(List<Points> Limb, int StartLocation, out int NextLocation)
		{
			NextLocation = -1;
			double num = 0.0;
			int num2 = StartLocation + 1;
			double d = Limb[StartLocation].D;
			double num3 = Limb[StartLocation].AA + 0.001;
			double num4 = Limb[StartLocation].AA + 0.1;
			do
			{
				if (num2 >= Limb.Count)
				{
					num2 = 0;
					num = 360.0;
				}
				if (Limb[num2].Valid && Limb[num2].AA + num > num3)
				{
					if (Limb[num2].AA + num > num4)
					{
						break;
					}
					if (Math.Abs(Limb[num2].D - d) < 0.01)
					{
						NextLocation = num2;
						break;
					}
				}
				num2++;
			}
			while (num2 < Limb.Count);
			if (NextLocation > 0)
			{
				return true;
			}
			return false;
		}

		internal static bool CheckForHigherPoints(List<Points> Limb, int StartLocation, int EndLocation, out int NewStartLocation)
		{
			double h = Limb[StartLocation].H;
			double num = Limb[EndLocation].H - h;
			double aA = Limb[StartLocation].AA;
			double num2 = Limb[EndLocation].AA;
			if (num2 < aA)
			{
				num2 = Limb.Count;
			}
			double num3 = num2 - aA;
			if (num3 < 0.0)
			{
				num3 += 360.0;
			}
			double num4 = -9.0;
			int num5 = -9;
			int num6 = 0;
			int num7 = 0;
			num6 = StartLocation;
			num7 = ((StartLocation >= EndLocation) ? Limb.Count : EndLocation);
			double num8 = -9.0;
			for (int i = 0; i <= 1; i++)
			{
				for (int j = num6; j < num7; j++)
				{
					if (Limb[j].Valid && !((Limb[j].AA == aA) | (Limb[j].AA == num2)))
					{
						double num9 = Limb[j].AA - aA;
						if (num9 < 0.0)
						{
							num9 += 360.0;
						}
						num8 = Limb[j].H - (h + num9 / num3 * num);
						if (num8 > 0.0 && num8 > num4)
						{
							num4 = num8;
							num5 = j;
						}
					}
				}
				if (StartLocation < EndLocation)
				{
					break;
				}
				num6 = 0;
				num7 = EndLocation;
			}
			NewStartLocation = num5;
			if (num5 < 0)
			{
				return false;
			}
			return true;
		}

		internal static void MarkAsInvalid(List<Points> Limb)
		{
			int count = Limb.Count;
			int num = 2000;
			double num2 = -9.0;
			double num3 = -9.0;
			int num4 = -1;
			int num5 = 0;
			int num6 = 0;
			do
			{
				num2 = (num3 = -9.0);
				num = (int)(1000.0 + 3000.0 * Math.Abs(Math.Sin(Limb[num6].AA / (180.0 / Math.PI))));
				for (int i = 0; i <= num; i++)
				{
					num5 = i + num6;
					if (num5 >= count)
					{
						break;
					}
					if (Limb[num5].H > num2)
					{
						num3 = num2;
						num2 = Limb[num5].H;
						num4 = num5;
					}
				}
				if (num2 - num3 > 0.04 && num4 > num6 + (int)(0.25 * (double)num - 2.0) && num4 < num6 + (int)(0.75 * (double)num + 2.0))
				{
					Limb[num4].Valid = false;
				}
				num6 += num / 2;
			}
			while (num6 < count);
		}
	}
}
