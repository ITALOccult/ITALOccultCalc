namespace Occult
{
	public class JamesWeb
	{
		internal static void GetJWST_OffsetFromEarth(double JD_TT, double xEarth, double yEarth, double zEarth, out double jX, out double jY, out double jZ)
		{
			jX = (jY = (jZ = 0.0));
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			num = 0.01000475470002796 * xEarth;
			num2 = 0.01000475470002796 * yEarth;
			num3 = 0.01000475470002796 * zEarth;
			jX = num;
			jY = num2;
			jZ = num3;
		}
	}
}
