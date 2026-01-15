using System;
using System.Text;

namespace Occult
{
	internal class HIPsource : IComparable
	{
		private const double EpochDiff = 24.75;

		private const double MilliSec_InRadian = 648000000.0 / Math.PI;

		private double ra_2016;

		private double raCosDec_2016;

		private double dec;

		private double mag;

		private double cosDecFactor;

		private int catNumber;

		private string hipLine = "";

		private string catID = "";

		private string catNum = "";

		internal static bool SortByRA_NotHip = true;

		internal string HipLine
		{
			set
			{
				hipLine = value;
				string[] array = hipLine.Split(new char[1] { ',' });
				dec = double.Parse(array[2]) + 24.75 * double.Parse(array[5]);
				cosDecFactor = Math.Cos(dec / (648000000.0 / Math.PI));
				ra_2016 = double.Parse(array[1]) + 24.75 * double.Parse(array[4]) / cosDecFactor;
				raCosDec_2016 = ra_2016 * cosDecFactor;
				mag = double.Parse(array[6]);
				catID = "2";
				catNum = array[0];
				catNumber = int.Parse(catNum);
			}
		}

		internal double RA_2016 => ra_2016;

		internal double CosDecFactor => cosDecFactor;

		internal double RACosDec_2016 => raCosDec_2016;

		internal double Dec_2016 => dec;

		internal double Mag => mag;

		internal string CatID => catID;

		internal string CatNum => catNum;

		internal int CatNumber => catNumber;

		public int CompareTo(object other)
		{
			if (SortByRA_NotHip)
			{
				return RA_2016.CompareTo(((HIPsource)other).RA_2016);
			}
			return CatNumber.CompareTo(((HIPsource)other).CatNumber);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			string[] array = hipLine.Split(new char[1] { ',' });
			stringBuilder.Append(array[1] + "," + array[2] + "," + array[3] + "," + array[4] + "," + array[5] + ",0,-8.75,");
			double.TryParse(array[6], out var result);
			double.TryParse(array[7], out var result2);
			double.TryParse(array[8], out var result3);
			stringBuilder.AppendFormat("{0,1:f2},{1,1:f2},{2,1:f2},", result2 + result, result, result - result3);
			stringBuilder.Append(array[9] + "," + array[10] + "," + array[11] + "," + array[12] + "," + array[13] + ",0,");
			double.TryParse(array[14], out var result4);
			int.TryParse(array[15], out var result5);
			int.TryParse(array[16], out var result6);
			int num = result6 % 10;
			double num2 = result5 - num;
			double num3 = Math.Sqrt(Math.Pow(Math.Sqrt(2.0 / (9.0 * num2)) * result4 + 1.0 - 2.0 / (9.0 * num2), 3.0));
			int num4 = (result6 - num) / 10;
			int num5 = 0;
			if ((num4 == 1 || num4 == 4) | "DWXZ".Contains(array[17].Trim()) | (array[18].Trim() != "1"))
			{
				num5 = 1;
			}
			stringBuilder.AppendFormat("{0,2:f2},{1,1:f1},0,", num3, num5);
			stringBuilder.Append("0,");
			stringBuilder.Append(array[0] + ",");
			stringBuilder.Append(catID + "," + catNum);
			return stringBuilder.ToString();
		}
	}
}
