using System;

namespace Occult
{
	public static class MatrixOps
	{
		internal const double Radian = 180.0 / Math.PI;

		public static Vector CrossProduct(Vector V1, Vector V2)
		{
			return new Vector
			{
				X = V1.Y * V2.Z - V1.Z * V2.Y,
				Y = V1.Z * V2.X - V1.X * V2.Z,
				Z = V1.X * V2.Y - V1.Y * V2.X
			};
		}

		public static double DotProduct(Vector V1, Vector V2)
		{
			return V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;
		}

		public static Vector Addition(Vector V1, Vector V2)
		{
			return new Vector
			{
				X = V1.X + V2.X,
				Y = V1.Y + V2.Y,
				Z = V1.Z + V2.Z
			};
		}

		public static Vector Subtraction(Vector V1, Vector V2)
		{
			return new Vector
			{
				X = V1.X - V2.X,
				Y = V1.Y - V2.Y,
				Z = V1.Z - V2.Z
			};
		}

		public static Vector Multiplication(Vector V1, double Factor)
		{
			return new Vector
			{
				X = V1.X * Factor,
				Y = V1.Y * Factor,
				Z = V1.Z * Factor
			};
		}

		public static Vector Division(Vector V1, double Factor)
		{
			return new Vector
			{
				X = V1.X / Factor,
				Y = V1.Y / Factor,
				Z = V1.Z / Factor
			};
		}

		public static Vector Rotation(Matrix R, Vector V)
		{
			return new Vector
			{
				X = R.R1C1 * V.X + R.R1C2 * V.Y + R.R1C3 * V.Z,
				Y = R.R2C1 * V.X + R.R2C2 * V.Y + R.R2C3 * V.Z,
				Z = R.R3C1 * V.X + R.R3C2 * V.Y + R.R3C3 * V.Z
			};
		}

		public static Vector UnitVector(Vector R)
		{
			Vector vector = new Vector();
			double num = Math.Sqrt(R.X * R.X + R.Y * R.Y + R.Z * R.Z);
			vector.SetVectorValues(R.X / num, R.Y / num, R.Z / num);
			return vector;
		}

		public static double Modulus(Vector R)
		{
			return Math.Sqrt(R.X * R.X + R.Y * R.Y + R.Z * R.Z);
		}

		public static void SphericalCoords(Vector R, out double Longitude, out double Latitude, out double Distance)
		{
			Longitude = Math.Atan2(R.Y, R.X);
			Latitude = Math.Atan2(R.Z, Math.Sqrt(R.X * R.X + R.Y * R.Y));
			Distance = Modulus(R);
		}
	}
}
